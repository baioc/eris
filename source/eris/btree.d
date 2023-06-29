/// Generic B-tree data structure.
module eris.btree;

import std.typecons : Ternary;

import eris.allocator : isAllocator, Mallocator, make, dispose;


/// Static parameters for [BTree] template.
struct BTreeParameters {
	/// Whether or not to allow duplicates in the B-tree; defaults to `false`.
	bool allowDuplicates = false;

	/// Max elements stored per node, must be at least 2. Use `size_t.max` for default.
	size_t nodeSlots = size_t.max;

	/// Whether to use a binary intra-node search. Use `Ternary.unknown` for default.
	Ternary useBinarySearch = Ternary.unknown;

	/++
	Use a custom comparison function as an alternative to static
	[opCmp](https://dlang.org/spec/operatoroverloading.html#compare).
	+/
	bool customCompare = false;
}

// TODO: treat all T* as void* in user-facing templates -> should reduce bloat


/++
B-tree data structure template.

B-trees are optimized for "cache friendliness" and low overhead per stored element.
The main tradeoff w.r.t other self-balancing trees is that $(B insertions and
deletions from a B-tree will move multiple elements around per operation,
invalidating references and iterators).
When elements are big, consider storing them through indirect references.

If duplicate elements are allowed in the B-tree, they have unspecified order among themselves.
+/
struct BTree(T, BTreeParameters params = BTreeParameters.init, Allocator = Mallocator)
if (isAllocator!Allocator)
{
 private:
	import std.algorithm.comparison : max;
	import eris.core : move;

	static struct NodeMetaData {
		import std.bitmanip : bitfields;
		debug { // XXX: bitfields-as-a-lib doesn't play well with GDB
			uint slotsInUse;
			bool isInternal;
		} else {
			mixin(bitfields!(
				uint, "slotsInUse", 31,
				bool, "isInternal", 1)
			);
		}
		uint indexInParent = uint.max;
		InternalNode* parent = null;
	}

	// more slots per node mean better locality in leaves, so we probably want a
	// multiple of cache line size, 256 (-metadata) was experimentally chosen
	enum size_t nodeSlots = params.nodeSlots != size_t.max
		? params.nodeSlots
		: max(2, (256 - NodeMetaData.sizeof) / T.sizeof);
	static assert(nodeSlots >= 2, "B-trees must have at least 2 elements per node");

	// linear is faster than binary search for small arrays, so we only default
	// to bsearch when our givenNodeSlots > 2 * defaultNodeSlots(u64)
	enum size_t bsearchThreshold = 2 * (256 - NodeMetaData.sizeof) / double.sizeof;
	enum bool useBinarySearch =
		params.useBinarySearch == Ternary.yes
		|| (params.useBinarySearch == Ternary.unknown && nodeSlots > bsearchThreshold);

 private:
	static struct TreeNode {
		NodeMetaData metadata;
		T[nodeSlots] slots;
		alias metadata this;
		static TreeNode opCall(bool isInternal = false) {
			TreeNode n;
			n.isInternal = isInternal;
			return n;
		}
	}
	static assert(TreeNode.init.isInternal == false);
	static assert(TreeNode.init.slotsInUse == 0);
	static assert(TreeNode.init.parent == null);

	static struct InternalNode {
		TreeNode asTreeNode = TreeNode(true);
		TreeNode*[nodeSlots+1] children;
		alias asTreeNode this;
	}
	static assert(InternalNode.init.isInternal == true);
	static assert(InternalNode.asTreeNode.offsetof == 0);

 private:
	TreeNode* root = null;
	invariant(root == null || root.parent == null);

	size_t totalInUse = 0;

	static if (params.customCompare) {
		int delegate(ref const(T), ref const(T)) nothrow @nogc compare = null;
		invariant(compare != null, "custom comparison can't be null");
	} else {
		import eris.core : compare = opCmp;
	}

	Allocator allocator;

 public:
	static if (params.customCompare) {
		@disable this();
		// Constructor taking in a custom ordering function and allocator.
		this(int delegate(ref const(T), ref const(T)) nothrow @nogc opCmp, Allocator alloc = Allocator.init) {
			this.compare = opCmp;
			this.allocator = alloc;
		}
	} else static if (!is(Allocator == Mallocator)) {
		@disable this();
		// Constructor taking in a custom allocator.
		this(Allocator alloc) {
			this.allocator = alloc;
		}
	}

	/++
	Removes all elements and deallocates all memory for this tree.

	If existing elements are structs with a destructor defined, those will be called.
	+/
	void clear()
	out (; this.length == 0)
	{
		this.deallocate(this.root);
		this.root = null;
		this.totalInUse = 0;
	}

	/// Check if the tree contains an element.
	bool opBinaryRight(string op : "in")(in T x) const => (this.opIndex(x) != null);

	/++
	Gets the address of the first matching element, or `null` if it isn't in the tree.

	NOTE: returned pointer may be invalidated by any insertions or deletions.
	+/
	inout(T)* opIndex(in T x) inout => this.search(this.root, x);

	/// Returns the number of elements currently stored in the tree.
	@property size_t length() const => this.totalInUse;

	/++
	Adds an element to the tree.

	If the tree allows duplicates, this is an insertion; if it doesn't, this is an upsert.

	NOTE: $(LIST
		* Returned pointer may be invalidated by any following insertions or deletions.
		* Allocation failure is irrecoverable in this implementation.
	)

	Returns: A pointer to the stored element, or `null` in case of insertion failure.
	+/
	T* put(T x) {
		static if (params.allowDuplicates)
			return this.upsert(x, () => move(x));
		else
			return this.upsert(x, () => move(x), (ref old){ move(x, old); });
	}

	/++
	Updates an element already in the set or creates a new one therein.

	NOTE: $(LIST
		* Returned pointer may be invalidated by any following insertions or deletions.
		* Allocation failure is irrecoverable in this implementation.
	)

	Params:
		x = element being looked up
		create = callback to create a new matching element to insert
		update = callback to modify an existing element in the tree; only called
			if not `null`; should never be provided if the tree allows duplicates

	Returns:
		A pointer to the element currently stored in the set, whether it was updated or inserted.
		This value is only `null` if the element would have been inserted but
		the operation didn't succeed at some point (i.e. memory allocation failure).
	+/
	T* upsert(
		in T x,
		scope T delegate() nothrow @nogc create,
		scope void delegate(ref T) nothrow @nogc update = null
	)
	in {
		assert(create != null);
		static if (params.allowDuplicates) assert(update == null);
	} out (p) {
		assert((p == null) || (*p in this));
	} do {
		// root node is lazily allocated as a leaf (at first)
		if (this.root == null) {
			this.root = allocator.make!TreeNode();
			if (this.root == null) return null;
		}

		// then we insert normally
		TreeNode* splitNode = null;
		T* inserted = insert(this.root, x, create, update, splitNode, this.totalInUse);
		if (splitNode == null) return inserted;

		// whenever a split bubbles up to the root, we create a new root with
		// a single element (the median) and two children (left and right nodes)
		TreeNode* oldRoot = this.root;
		auto newRoot = allocator.make!InternalNode();
		if (newRoot == null) return null; // XXX: can't undo split(s)
		newRoot.slotsInUse = 1;
		move(oldRoot.slots[oldRoot.slotsInUse - 1], newRoot.slots[0]);
		const bool moveInserted = (&oldRoot.slots[oldRoot.slotsInUse - 1] == inserted);
		oldRoot.slotsInUse = oldRoot.slotsInUse - 1;
		setChild(newRoot, 0, oldRoot);
		setChild(newRoot, 1, splitNode);
		this.root = &newRoot.asTreeNode;
		return moveInserted ? &newRoot.slots[0] : inserted;
	}

	/++
	Removes (at most) one element from the tree, if it's there.

	Params:
		x = element being looked up in the tree
		destroy = callback to cleanup after the element being removed (if found);
			defaults to [object.destroy](https://dlang.org/library/object/destroy.html).

	Returns: Whether or not at least one `x` was contained in the tree.
	+/
	bool remove(in T x, scope void delegate(ref T) nothrow @nogc destroy = null) {
		if (this.root == null) return false;
		void delegate(ref T) nothrow @nogc dg = destroy == null ? (ref x){ .destroy(x); } : destroy;
		return this.searchAndRemove(this.root, x, dg);
	}

	/// Iterates over elements in order. NOTE: should not be used while inserting or deleting elements from the tree.
	int opApply(scope int delegate(ref T) nothrow @nogc dg) => opApply(this.root, dg);

	/// ditto
	int opApply(scope int delegate(ref const(T)) nothrow @nogc dg) const {
		return (cast(BTree) this).opApply(cast(int delegate(ref T) nothrow @nogc) dg);
	}

	// TODO: actually, define opCmp instead, with external iterators
	/// Element-wise comparison.
	bool opEquals(BTreeParameters P, A)(ref const(BTree!(T,P,A)) other) const {
		if (&this == cast(typeof(&this)) &other) return true;
		if (this.length != other.length) return false;
		foreach (ref const x; this) if (x !in other) return false;
		return true;
	}

	/// Combined element hash.
	size_t toHash() const {
		size_t hash = 0;
		foreach (ref const x; this) hash = .hashOf(x, hash);
		return hash;
	}

 private:
	import eris.array : shift, shiftLeft;

	void deallocate(TreeNode* node) {
		import std.traits : hasElaborateDestructor;
		if (node == null) return;
		static if (hasElaborateDestructor!T) {
			foreach (ref slot; node.slots[0 .. node.slotsInUse]) destroy(slot);
		}
		if (node.isInternal) {
			auto internal = cast(InternalNode*) node;
			foreach (int i; 0 .. internal.slotsInUse + 1) {
				deallocate(internal.children[i]);
				internal.children[i] = null;
			}
			this.allocator.dispose(internal);
		} else {
			this.allocator.dispose(node);
		}
	}

	// returns the leftmost index i such that `array[0 .. |i|) < x <= array[|i| .. $)`;
	// this index will have its sign bit set to 1 iff `i == array.length || x < array[i]`
	// (i.e., the element was not found) and to 0 if x is at `haystack[i]`.
	// NOTE: do NOT clear the sign bit with negation, as `int.min` is a very real possibility.
	int bisect(in T[] array, in T x) const
	out (pos) {
		if (pos >= 0) assert(compare(array[pos], x) == 0);
		else assert((pos & ~(1 << 31)) <= array.length);
	} do {
		static assert(nodeSlots < int.max);

		static if (useBinarySearch) {
			int begin = 0;
			int end = cast(int) array.length;
			while (end - begin >= 1) {
				const mid = begin + (end - begin)/2;
				const cmp = compare(x, array[mid]);
				if (cmp < 0) end = mid;
				else if (cmp == 0) return mid;
				else if (cmp > 0) begin = mid + 1;
			}
			return begin | (1 << 31);

		} else /* use linear search */ {
			int i = 0;
			for (; i < array.length; ++i) {
				const cmp = compare(array[i], x);
				if (cmp < 0) continue;
				if (cmp == 0) return i;
				if (cmp > 0) break;
			}
			return i | (1 << 31);
		}
	}

	inout(T)* search(inout(TreeNode*) node, in T key) inout {
		// search this node's slots (if not null)
		if (node == null) return null;
		int pos = bisect(node.slots[0 .. node.slotsInUse], key);
		// on leaf nodes, we either found it or we give up
		if (pos >= 0) return &node.slots[pos];
		else if (!node.isInternal) return null;
		// but on internal nodes, we tail-recursively search a specific child
		auto internalNode = cast(inout(InternalNode*)) node;
		int child = pos & ~(1 << 31);
		return search(internalNode.children[child], key);
	}

	// update an existing element or create a matching one and add it to the tree
	T* insert(
		TreeNode* node,
		in T x,
		scope T delegate() nothrow @nogc create,
		scope void delegate(ref T) nothrow @nogc update,
		out TreeNode* splitNode,
		ref size_t totalInUse
	)
	in (node != null && create != null)
	{
		// search this (non-null) node's slots
		int pos = bisect(node.slots[0 .. node.slotsInUse], x);

		// if found, just return the right address (when no duplicates)
		static if (!params.allowDuplicates) {
			if (pos >= 0) {
				if (update != null) update(node.slots[pos]);
				return &node.slots[pos];
			}
			pos &= ~(1 << 31);
		} else { // if duplicates are allowed, we always insert
			if (pos < 0) pos &= ~(1 << 31);
		}

		// when a leaf is reached and has space, insert right there
		if (!node.isInternal && node.slotsInUse < nodeSlots) {
			T created = create();
			totalInUse++;
			node.slotsInUse = node.slotsInUse + 1;
			shift(node.slots[0 .. node.slotsInUse], pos, created);
			return &node.slots[pos];
		}

		// when the leaf has no slots left, we'll split it in two
		if (!node.isInternal && node.slotsInUse == nodeSlots) {
			// we'll always create a new right node, but the split is not so
			// trivial because there's still a pending insertion
			splitNode = this.allocator.make!TreeNode();
			if (splitNode == null) return null;
			T created = create();
			totalInUse++;
			return split(node, splitNode, created, pos);
		}

		// these last cases handle an internal node which doesnt't itself contain
		// x, so we know that the insertion takes place in one of its children
		auto internalNode = cast(InternalNode*) node;
		TreeNode* child = internalNode.children[pos];
		T* inserted = insert(child, x, create, update, splitNode, totalInUse);
		if (splitNode == null) return inserted; // child wasn't split

		// if the child was split, we'll move its median element (last of the
		// left child) into this node and insert the new right child pointer
		// NOTE: ended up moving the inserted element? adjust returned address
		T median = move(child.slots[child.slotsInUse - 1]);
		const bool moveInserted = (&child.slots[child.slotsInUse - 1] == inserted);
		child.slotsInUse = child.slotsInUse - 1;
		pos = bisect(internalNode.slots[0 .. internalNode.slotsInUse], median);
		assert(pos < 0);
		pos &= ~(1 << 31);

		// which is easy when there's space in this node ...
		if (internalNode.slotsInUse < nodeSlots) {
			internalNode.slotsInUse = internalNode.slotsInUse + 1;
			shift(internalNode.slots[0 .. internalNode.slotsInUse], pos, median);
			shiftChild(internalNode, pos + 1, splitNode);
			splitNode = null;
			return moveInserted ? &internalNode.slots[pos] : inserted;
		}

		// .. but when there are no slots left, another split is needed at this level
		auto newNode = this.allocator.make!InternalNode();
		if (newNode == null) return null; // XXX: can't undo split(s)
		T* splitInsert = split(internalNode, newNode, median, pos, splitNode);
		splitNode = &newNode.asTreeNode;
		return moveInserted ? splitInsert : inserted;
	}

	// recursively search for an element to delete
	bool searchAndRemove(TreeNode* node, in T x, scope void delegate(ref T) nothrow @nogc destroy)
	in (node != null && destroy != null)
	{
		// if the element is found at this level, destroy it and clear the slot
		int pos = bisect(node.slots[0 .. node.slotsInUse], x);
		if (pos >= 0) {
			destroy(node.slots[pos]);
			this.totalInUse -= 1;
			clearSlot(node, pos);
			return true;
		}
		// at leafs, declare not found,
		if (!node.isInternal) return false;
		// but try deeper at internal nodes
		auto internalNode = cast(InternalNode*) node;
		const int child = pos & ~(1 << 31);
		bool found = searchAndRemove(internalNode.children[child], x, destroy);
		if (!found) return false;
		rebalanceIfNeeded(internalNode, child);
		return true;
	}

	// adjust a node just after destroying an element at the given slot
	void clearSlot(TreeNode* node, int pos) {
		// at leaf nodes, we just have to shift slots
		if (!node.isInternal) {
			shiftLeft(node.slots[pos .. node.slotsInUse]);
			node.slotsInUse = node.slotsInUse - 1;
			return;
		}
		// else, steal biggest element of the left *subtree* and rebalance on the way up
		auto parent = cast(InternalNode*) node;
		void removeBiggest(TreeNode* current) {
			assert(current != null);
			if (!current.isInternal) {
				move(current.slots[current.slotsInUse - 1] , parent.slots[pos]);
				current.slotsInUse = current.slotsInUse - 1;
			} else {
				auto internalNode = cast(InternalNode*) current;
				removeBiggest(internalNode.children[internalNode.slotsInUse]);
				rebalanceIfNeeded(internalNode, internalNode.slotsInUse);
			}
		}
		TreeNode* leftSubTree = parent.children[pos];
		removeBiggest(leftSubTree);
		rebalanceIfNeeded(parent, pos);
	}

	// rebalance after removing *at most one* element from the given child node
	void rebalanceIfNeeded(InternalNode* parent, int childIndex)
	in (parent != null && childIndex >= 0 && childIndex <= parent.slotsInUse)
	{
		TreeNode* child = parent.children[childIndex];
		assert(child != null);

		// leafs only need rebalancing when they're completely empty
		if (!child.isInternal && child.slotsInUse >= 1) return;

		// we can also skip internal nodes with enough remaining load
		enum uint minLoad = nodeSlots / 2;
		if (child.isInternal && child.slotsInUse >= minLoad) return;

		// at this point, we know the child is deficient, so we'll start by
		// checking if its siblings (when they exist) have spare slots to give
		TreeNode* leftSibling = childIndex > 0 ? parent.children[childIndex - 1] : null;
		TreeNode* rightSibling = childIndex < parent.slotsInUse ? parent.children[childIndex + 1] : null;
		bool existsAndHasSpare(TreeNode* node) {
			if (node == null) return false;
			else if (node.isInternal) return node.slotsInUse > 1;
			else return node.slotsInUse > minLoad;
		}

		// rotate left
		if (existsAndHasSpare(rightSibling)) {
			move(parent.slots[childIndex], child.slots[child.slotsInUse]);
			move(rightSibling.slots[0], parent.slots[childIndex]);
			shiftLeft(rightSibling.slots[0 .. rightSibling.slotsInUse]);
			if (child.isInternal) {
				auto childAsInternal = cast(InternalNode*) child;
				auto rightAsInternal = cast(InternalNode*) rightSibling;
				TreeNode* sparePointer = move(rightAsInternal.children[0]);
				for (int i = 0; i + 1 < rightSibling.slotsInUse + 1; ++i) {
					setChild(rightAsInternal, i, move(rightAsInternal.children[i + 1]));
				}
				setChild(childAsInternal, child.slotsInUse + 1, sparePointer);
			}
			child.slotsInUse = child.slotsInUse + 1;
			rightSibling.slotsInUse = rightSibling.slotsInUse - 1;

		// rotate right
		} else if (existsAndHasSpare(leftSibling)) {
			child.slotsInUse = child.slotsInUse + 1;
			shift(child.slots[0 .. child.slotsInUse], 0, parent.slots[childIndex - 1]);
			move(leftSibling.slots[leftSibling.slotsInUse - 1], parent.slots[childIndex - 1]);
			if (child.isInternal) {
				auto leftAsInternal = cast(InternalNode*) leftSibling;
				auto childAsInternal = cast(InternalNode*) child;
				shiftChild(childAsInternal, 0, move(leftAsInternal.children[leftSibling.slotsInUse]));
			}
			leftSibling.slotsInUse = leftSibling.slotsInUse - 1;

		// merge two side-by-side children and delete the one on the right
		} else {
			TreeNode* left, right;
			int separatorIndex = -1;
			if (rightSibling != null) {
				left = child;
				right = rightSibling;
				separatorIndex = childIndex;
			} else {
				assert(leftSibling != null);
				left = leftSibling;
				right = child;
				separatorIndex = childIndex - 1;
			}
			assert(left.slotsInUse + right.slotsInUse < nodeSlots);

			// separator element in parent moves down
			move(parent.slots[separatorIndex], left.slots[left.slotsInUse]);
			shiftLeft(parent.slots[separatorIndex .. parent.slotsInUse]);
			for (int i = separatorIndex + 1; i + 1 < parent.slotsInUse + 1; ++i) {
				setChild(parent, i, move(parent.children[i + 1]));
			}
			parent.slotsInUse = parent.slotsInUse - 1;

			// merge left <- right
			foreach (int i; 0 .. right.slotsInUse) {
				move(right.slots[i], left.slots[left.slotsInUse + 1 + i]);
			}
			if (child.isInternal) {
				auto leftAsInternal = cast(InternalNode*) left;
				auto rightAsInternal = cast(InternalNode*) right;
				foreach (int i; 0 .. right.slotsInUse + 1) {
					setChild(leftAsInternal, left.slotsInUse + 1 + i, move(rightAsInternal.children[i]));
				}
			}
			left.slotsInUse = left.slotsInUse + 1 + right.slotsInUse;
			if (right.isInternal) {
				this.allocator.dispose(cast(InternalNode*) right);
			} else {
				this.allocator.dispose(right);
			}
			// ^ good citizenship: deallocate the correct type

			// if the root just became empty, delete it and make the tree shallower
			// by using the merged node as the new root; else just rebalance the
			// parent (should happen as we go back up the call stack)
			if (&parent.asTreeNode == this.root && this.root.slotsInUse == 0) {
				this.allocator.dispose(parent);
				this.root = left;
			}
		}
	}

 static:
	void setChild(InternalNode* parent, uint index, TreeNode* child)
	in (parent != null && index <= nodeSlots && child != null)
	{
		parent.children[index] = child;
		child.indexInParent = index;
		child.parent = parent;
	}

	void shiftChild(InternalNode* parent, uint index, TreeNode* child)
	in (parent != null && index <= nodeSlots && child != null)
	{
		const int target = index;
		for (int i = parent.slotsInUse; i - 1 >= target; --i) {
			setChild(parent, i, move(parent.children[i - 1]));
		}
		setChild(parent, index, child);
	}

	// easier-to-call overloads for leaf and internal nodes (see template impl below)
	T* split(TreeNode* left, TreeNode* right, ref T x, int pos) {
		return splitImpl!false(left, right, x, pos, null);
	}
	T* split(InternalNode* left, InternalNode* right, ref T x, int pos, TreeNode* splitChild) {
		return splitImpl!true(&left.asTreeNode, &right.asTreeNode, x, pos, splitChild);
	}

	// split a left node's elements across it and a newly allocated right node,
	// with a pending insertion of x at index `pos` in the original (full) node
	T* splitImpl(bool isInternal)(
		TreeNode* left,
		TreeNode* right,
		ref T x,
		int pos,
		TreeNode* splitChild
	)
	in {
		assert(left != null);
		assert(right != null);
		assert(left.isInternal == isInternal);
		assert(right.isInternal == isInternal);
		assert(left.slotsInUse == nodeSlots);
		assert(right.slotsInUse == 0);
		assert(pos >= 0);
		assert(pos <= nodeSlots);
		assert(isInternal == (splitChild != null));
	} out (_) {
		assert(left.slotsInUse <= nodeSlots);
		assert(right.slotsInUse <= nodeSlots);
	} do {
		T* inserted;
		const int mid = nodeSlots / 2;
		auto leftInternal = cast(InternalNode*) left;
		auto rightInternal = cast(InternalNode*) right;

		if (pos <= mid) {
			// for an insertion in the left node, set up the right node first
			foreach (int i; mid .. nodeSlots) {
				move(left.slots[i], right.slots[i - mid]);
				static if (isInternal) setChild(rightInternal, i-mid+1, move(leftInternal.children[i+1]));
			}
			right.slotsInUse = nodeSlots - mid;
			// then do the insertion on the left node
			left.slotsInUse = mid - 0 + 1;
			shift(left.slots[0 .. left.slotsInUse], pos, x);
			static if (isInternal) shiftChild(leftInternal, pos+1, splitChild);
			inserted = &left.slots[pos];

		} else /* if (pos > mid) */ {
			// for an insertion in the right node, we can keep the left mostly untouched
			int to = 0;
			foreach (int i; mid + 1 .. pos) {
				move(left.slots[i], right.slots[to]);
				static if (isInternal) setChild(rightInternal, to+1, move(leftInternal.children[i+1]));
				++to;
			}
			move(x, right.slots[to]);
			static if (isInternal) setChild(rightInternal, to+1, splitChild);
			inserted = &right.slots[to];
			++to;
			foreach (int i; pos .. nodeSlots) {
				move(left.slots[i], right.slots[to]);
				static if (isInternal) setChild(rightInternal, to+1, move(leftInternal.children[i+1]));
				++to;
			}
			// then simply adjust slot use count
			right.slotsInUse = to;
			left.slotsInUse = mid + 1 - 0;
		}

		// there's one last detail when splitting internal nodes: since this was
		// a split, we know that the last slot of the left node will be pushed
		// upwards to its parent, but the child pointer to the right of that node
		// needs to be put somewhere; that somewhere is precisely the first
		// pointer slot of the just-created right node, which should be null now
		static if (isInternal) {
			assert(rightInternal.children[0] == null);
			setChild(rightInternal, 0, move(leftInternal.children[left.slotsInUse]));
		}

		return inserted;
	}

	// in-order iteration over used slots
	int opApply(TreeNode* node, scope int delegate(ref T) nothrow @nogc dg) {
		if (node == null) return 0;
		const m = node.slotsInUse;
		if (node.isInternal) {
			auto internalNode = cast(InternalNode*) node;
			int stop = 0;
			for (int i = 0; i < m; ++i) {
				stop = opApply(internalNode.children[i], dg);
				if (stop) return stop;
				stop = dg(internalNode.slots[i]);
				if (stop) return stop;
			}
			return opApply(internalNode.children[m], dg);
		} else {
			foreach (ref x; node.slots[0 .. m]) {
				int stop = dg(x);
				if (stop) return stop;
			}
			return 0;
		}
	}
}


///
nothrow @nogc unittest {
	// tip: debug w/ visualizer at https://www.cs.usfca.edu/~galles/visualization/BTree.html
	enum BTreeParameters params = {
		nodeSlots: 3,
		customCompare: true,
		useBinarySearch: Ternary.yes,
	};
	auto btree = BTree!(int, params)((ref a, ref b) => a - b);
	static const payload = [
		34, 33, 38,
		28, 27, 22,
		30, 21, 24,
		18, 19, 20, 26, 32, 42,
		23,
	];

	assert(btree.length == 0);
	foreach (x; payload) {
		// at first, the set does not contain x
		assert(x !in btree);
		assert(!btree.remove(x));
		// insert x and test returned address
		int* p = btree.put(x);
		assert(*p == x);
		// now it does contain x, so test lookup
		assert(x in btree);
		assert(p == btree[x]);
		// redundant insert is redundant
		int* q = btree.put(x);
		assert(q == p);
	}

	// sanity check: the b-tree didn't come up with new elements we didn't insert
	assert(btree.length == payload.length);
	foreach (ref const x; btree) {
		import std.algorithm.searching : canFind;
		assert(payload.canFind(x));
	}

	// remove all but the last 3 elements in reverse order
	for (int n = 1; n + 2 < payload.length; ++n) {
		auto x = payload[$ - n];
		assert(x in btree);
		btree.remove(x);
		assert(x !in btree);
		assert(btree.length == payload.length - n);
	}
	assert(btree.length == 3);

	// make sure we test aggregate equality
	auto other = BTree!int();
	assert(btree != other);
	import std.range.primitives : put;
	static const remaining = [33, 34, 38];
	put(other, remaining);
	assert(btree == other);

	// clear gets rid of the rest
	btree.clear();
	assert(btree.length == 0);
	foreach (x; payload) assert(x !in btree);
}

///
nothrow @nogc unittest {
	static struct S {
	 nothrow @nogc:
		int x;
		uint discriminator = 0;
		alias x this;
		int opCmp(in S other) const => this.x - other.x;
	}

	enum BTreeParameters params = { allowDuplicates: true };
	BTree!(S, params) btree;
	static const payload = [ S(6), S(2), S(4), S(5), S(6, 9), S(4, 20) ];

	assert(btree.length == 0);
	foreach (x; payload) {
		S* p = btree.put(x);
		assert(*p == x);
		assert(x in btree);
	}
	assert(btree.length == payload.length);

	assert(S(4) in btree);
	assert(S(4, 20) in btree);
	assert(S(6) in btree);
	assert(S(6, 9) in btree);
}
