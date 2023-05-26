/// Generic B-tree data structure.
module eris.btree;

import std.typecons : Ternary;


/// Static parameters for [BTree] template.
struct BTreeParameters {
	/// Max elements stored per node, must be at least 2. Use `size_t.max` for default.
	size_t nodeSlots = size_t.max;

	/// Whether to use a binary intra-node search. Use `Ternary.unknown` for default.
	Ternary useBinarySearch = Ternary.unknown;

	/// Use a custom comparison function as an alternative to static [opCmp](https://dlang.org/spec/operatoroverloading.html#compare).
	bool customCompare = false;
}


// TODO: make this betterC-compatible
version (D_BetterC) {} else {

/++
Single-threaded B-tree data structure.

B-trees are optimized for "cache friendliness" and low memory overhead per stored element.
The main tradeoff w.r.t other self-balancing trees is that $(B insertions and deletions from a
B-tree will move multiple elements around per operation, invalidating references and iterators).
When elements are big, consider storing them through indirect references.
+/
struct BTree(T, BTreeParameters params = BTreeParameters.init) {
 private:
	import std.algorithm.comparison : max;
	import std.algorithm.mutation : move;
	import eris.core : hash_t;

	// more slots per node mean better locality in leaves, so we probably want a
	// multiple of cache line size, 256 (-4 for metada) was empirically chosen
	enum size_t nodeSlots =
		params.nodeSlots != size_t.max ? params.nodeSlots : max(2, (256 - 4) / T.sizeof);
	static assert(nodeSlots >= 2);

	// default bsearch threshold is simply 2x that size
	enum size_t binarySearchThreshold = max(4, (512 - 4) / T.sizeof);
	enum bool useBinarySearch =
		params.useBinarySearch == Ternary.yes
		|| (params.useBinarySearch == Ternary.unknown && nodeSlots > binarySearchThreshold);

 private:
	struct TreeNode {
		import std.bitmanip : bitfields;
		T[nodeSlots] slots;
		mixin(bitfields!(
			uint, "slotsInUse", 31,
			bool, "isInternal", 1)
		);
		static opCall(bool isInternal = false) {
			TreeNode n;
			n.isInternal = isInternal;
			return n;
		}
	}
	static assert(!TreeNode.init.isInternal);

	struct InternalNode {
		TreeNode asTreeNode = TreeNode(true);
		alias asTreeNode this;
		TreeNode*[nodeSlots+1] children;
	}
	static assert(InternalNode.init.isInternal);
	static assert(InternalNode.asTreeNode.offsetof == 0);

 private:
	TreeNode* root = null;
	size_t totalInUse = 0;
	static if (params.customCompare) {
		int delegate(ref const(T), ref const(T)) nothrow opCmp = null;
		invariant(opCmp != null, "custom comparison can't be null");
	}

 public:
	static if (params.customCompare) {
		@disable this();
		/// Constructor taking in a custom ordering function.
		this(int delegate(ref const(T), ref const(T)) nothrow opCmp) {
			this.opCmp = opCmp;
		}
	}

	/// Implements [eris.set.ExtensionalSet.clear]
	void clear() {
		// TODO: destroy if hasElaborateDestructor!T
		this.root = null;
		this.totalInUse = 0;
	}

	/// Implements [eris.set.ExtensionalSet.contains]
	pragma(inline)
	bool opBinaryRight(string op : "in")(in T x) const => this[x] != null;

	/// Implements [eris.set.ExtensionalSet.length]
	pragma(inline)
	@property size_t length() const => this.totalInUse;

	/// Iterates over elements in order. NOTE: should not be used while inserting or deleting elements from the tree.
	pragma(inline)
	int opApply(scope int delegate(ref T) nothrow dg) => opApply(this.root, dg);

	/// ditto
	pragma(inline)
	int opApply(scope int delegate(ref const(T)) nothrow dg) const {
		return (cast(BTree) this).opApply(cast(int delegate(ref T) nothrow) dg);
	}

	/// Implements [eris.set.ExtensionalSet.at]. NOTE: returned pointer may be invalidated by any insertions or deletions.
	pragma(inline)
	inout(T)* opIndex(in T x) inout => search(this.root, x);

	/// Implements [eris.set.MutableSet.upsert]. NOTE: returned pointer may be invalidated by any following insertions or deletions.
	T* upsert(in T x, scope T delegate() nothrow create, scope void delegate(ref T) nothrow update = null)
	in (create != null)
	out (p; (x in this) == (p != null))
	{
		// root node is lazily allocated as a leaf (at first)
		if (this.root == null) this.root = new TreeNode();

		// then insert normally
		TreeNode* splitNode = null;
		T* inserted = insert(this.root, x, create, update, splitNode, this.totalInUse);
		if (splitNode == null) return inserted;

		// whenever a split bubbles up to the root, we create a new root with
		// a single element (the median) and two children (left and right nodes)
		TreeNode* oldRoot = this.root;
		auto newRoot = new InternalNode();
		newRoot.slotsInUse = 1;
		move(oldRoot.slots[oldRoot.slotsInUse - 1], newRoot.slots[0]);
		const bool moveInserted = &oldRoot.slots[oldRoot.slotsInUse - 1] == inserted;
		oldRoot.slotsInUse = oldRoot.slotsInUse - 1;
		newRoot.children[0] = oldRoot;
		newRoot.children[1] = splitNode;
		this.root = &newRoot.asTreeNode;
		return moveInserted ? &newRoot.slots[0] : inserted;
	}

	/// Implements [eris.set.MutableSet.put]. NOTE: returned pointer may be invalidated by any following insertions or deletions.
	pragma(inline)
	T* put(T x) => this.upsert(x, () => move(x), (ref old){ move(x, old); });

	/// Implements [eris.set.MutableSet.remove]
	pragma(inline)
	bool remove(in T x, scope void delegate(ref T) nothrow destroy = null) {
		if (this.root == null) return false;
		void delegate(ref T) nothrow dg = destroy == null ? (ref x){ .destroy(x); } : destroy;
		return searchAndRemove(this.root, x, dg);
	}

	/// Element-wise comparison.
	pragma(inline)
	bool opEquals(BTreeParameters P)(ref const(BTree!(T,P)) other) const {
		if (&this == cast(typeof(&this)) &other) return true;
		if (this.length != other.length) return false;
		foreach (ref const x; this) if (x !in other) return false;
		return true;
	}

	/// Combined element hash.
	pragma(inline)
	hash_t toHash() const {
		hash_t hash = 0;
		foreach (ref const x; this) hash = .hashOf(x, hash);
		return hash;
	}

 private:
	import eris.array : shift, shiftLeft, unshift;

	// performs a sorted search for a given needle x, returning the leftmost
	// index i such that haystack[0 .. |i|) < x <= haystack[|i| .. $); this index
	// will have its sign bit set to 1 iff x < haystack[i] (needle not found),
	// so a sign bit of 0 indicates that the needle was found at haystack[i];
	// NOTE: do NOT clear the sign bit with negation, as int.min is a possibility
	int bisect(in T[] haystack, in T needle) const
	out (pos) {
		if (pos >= 0) assert(haystack[pos] == needle);
		else assert((pos & ~(1 << 31)) <= haystack.length);
	} do {
		import eris.core : opCmp;
		static assert(nodeSlots < int.max);

		static if (useBinarySearch) {
			int begin = 0;
			int end = cast(int) haystack.length;
			while (end - begin >= 1) {
				const mid = begin + (end - begin)/2;
				static if (params.customCompare) {
					const cmp = this.opCmp(haystack[mid], needle);
				} else {
					const cmp = haystack[mid].opCmp(needle);
				}
				if (cmp < 0) begin = mid + 1;
				else if (cmp == 0) return mid;
				else if (cmp > 0) end = mid;
			}
			return begin | (1 << 31);

		} else /* use linear search */ {
			int i = 0;
			for (; i < haystack.length; ++i) {
				static if (params.customCompare) {
					const cmp = this.opCmp(haystack[i], needle);
				} else {
					const cmp = haystack[i].opCmp(needle);
				}
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
		scope T delegate() nothrow create,
		scope void delegate(ref T) nothrow update,
		out TreeNode* splitNode,
		ref size_t totalInUse
	)
	in (node != null && create != null)
	{
		// search this (non-null) node's slots
		int pos = bisect(node.slots[0 .. node.slotsInUse], x);

		// if found, just return the right address
		if (pos >= 0) {
			if (update != null) update(node.slots[pos]);
			return &node.slots[pos];
		}
		pos &= ~(1 << 31);

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
			T created = create();
			totalInUse++;
			splitNode = new TreeNode();
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
		const bool moveInserted = &child.slots[child.slotsInUse - 1] == inserted;
		child.slotsInUse = child.slotsInUse - 1;
		pos = bisect(internalNode.slots[0 .. internalNode.slotsInUse], median);
		assert(pos < 0);
		pos &= ~(1 << 31);

		// which is easy when there's space in this node ...
		if (internalNode.slotsInUse < nodeSlots) {
			internalNode.slotsInUse = internalNode.slotsInUse + 1;
			shift(internalNode.slots[0 .. internalNode.slotsInUse], pos, median);
			shift(internalNode.children[0 .. internalNode.slotsInUse + 1], pos + 1, splitNode);
			splitNode = null;
			return moveInserted ? &internalNode.slots[pos] : inserted;
		}

		// .. but when there are no slots left, another split is needed at this level
		auto newNode = new InternalNode();
		T* splitInsert = split(internalNode, newNode, median, pos, splitNode);
		splitNode = &newNode.asTreeNode;
		return moveInserted ? splitInsert : inserted;
	}

	// recursively search for an element to delete
	bool searchAndRemove(TreeNode* node, in T x, scope void delegate(ref T) nothrow destroy)
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
		TreeNode* left = parent.children[pos];
		removeBiggest(left);
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
		static assert(minLoad > 0);
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
				TreeNode* sparePointer = unshift(rightAsInternal.children[0 .. rightSibling.slotsInUse + 1], 0);
				childAsInternal.children[child.slotsInUse + 1] = sparePointer;
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
				TreeNode* sparePointer = move(leftAsInternal.children[leftSibling.slotsInUse]);
				shift(childAsInternal.children[0 .. child.slotsInUse + 1], 0, sparePointer);
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
			shiftLeft(parent.children[separatorIndex + 1 .. parent.slotsInUse + 1]);
			parent.slotsInUse = parent.slotsInUse - 1;

			// merge left <- right
			foreach (int i; 0 .. right.slotsInUse) {
				move(right.slots[i], left.slots[left.slotsInUse + 1 + i]);
			}
			if (child.isInternal) {
				auto leftAsInternal = cast(InternalNode*) left;
				auto rightAsInternal = cast(InternalNode*) right;
				foreach (int i; 0 .. right.slotsInUse + 1) {
					move(rightAsInternal.children[i], leftAsInternal.children[left.slotsInUse + 1 + i]);
				}
			}
			left.slotsInUse = left.slotsInUse + 1 + right.slotsInUse;
			right.slotsInUse = 0;
			// TODO: explicitly deallocate right node

			// if the root just became empty, delete it and make the tree shallower
			// by using the merged node as the new root; else just rebalance the
			// parent (should happen as we go back up the call stack)
			if (&parent.asTreeNode == this.root && this.root.slotsInUse == 0) {
				this.root = left;
				// TODO: explicitly deallocate old root
			}
		}
	}

 static:
	// in-order iteration over used slots
	int opApply(TreeNode* node, scope int delegate(ref T) nothrow dg) {
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

	// easier-to-call overloads for leaf and internal nodes (see template impl below)
	pragma(inline) {
		T* split(TreeNode* left, TreeNode* right, ref T x, int pos) {
			return splitImpl!false(left, right, x, pos, null);
		}
		T* split(InternalNode* left, InternalNode* right, ref T x, int pos, TreeNode* splitChild) {
			return splitImpl!true(&left.asTreeNode, &right.asTreeNode, x, pos, splitChild);
		}
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
	} out (;
		left.slotsInUse < nodeSlots && right.slotsInUse < nodeSlots
	) do {
		T* inserted;
		const int mid = nodeSlots / 2;
		auto leftInternal = cast(InternalNode*) left;
		auto rightInternal = cast(InternalNode*) right;

		if (pos <= mid) {
			// for an insertion in the left node, set up the right node first
			foreach (int i; mid .. nodeSlots) {
				move(left.slots[i], right.slots[i - mid]);
				static if (isInternal) move(leftInternal.children[i+1], rightInternal.children[i-mid+1]);
			}
			right.slotsInUse = nodeSlots - mid;
			// then do the insertion on the left node
			left.slotsInUse = mid - 0 + 1;
			shift(left.slots[0 .. left.slotsInUse], pos, x);
			static if (isInternal) shift(leftInternal.children[0 .. left.slotsInUse+1], pos+1, splitChild);
			inserted = &left.slots[pos];

		} else /* if (pos > mid) */ {
			// for an insertion in the right node, we can keep the left mostly untouched
			int to = 0;
			foreach (int i; mid + 1 .. pos) {
				move(left.slots[i], right.slots[to]);
				static if (isInternal) move(leftInternal.children[i+1], rightInternal.children[to+1]);
				++to;
			}
			move(x, right.slots[to]);
			static if (isInternal) move(splitChild, rightInternal.children[to+1]);
			inserted = &right.slots[to];
			++to;
			foreach (int i; pos .. nodeSlots) {
				move(left.slots[i], right.slots[to]);
				static if (isInternal) move(leftInternal.children[i+1], rightInternal.children[to+1]);
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
			move(leftInternal.children[left.slotsInUse], rightInternal.children[0]);
		}

		return inserted;
	}
}


///
nothrow /* TODO: @nogc */ unittest {
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

	// make sure we test the aggregate equality
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

}
