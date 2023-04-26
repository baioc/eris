/// Generic B-Tree data structure.
module eris.btree;

import eris.core : hash_t;


/// Static parameters for [BTree] template. Use `size_t.max` for defaults.
struct BTreeParameters {
	/// Maximum number of elements stored per node, must be at least 2.
	size_t nodeSlots = size_t.max;

	/// If `nodeSlots` is greater than this, use a binary intra-node search.
	size_t binarySearchThreshold = size_t.max;
}


// TODO: make this betterC-compatible
version (D_BetterC) {} else {

/// B-Tree template.
struct BTree(T, BTreeParameters params = BTreeParameters.init) {
 private:
	import std.algorithm.comparison : max;
	import std.algorithm.mutation : move;

	// since nodeSlots consist of a contiguous array of Ts, we can choose a
	// default based on L2 "Streamer" prefetch block size (128 B)
	enum size_t nodeSlots =
		params.nodeSlots != size_t.max ? params.nodeSlots : max(2, 128 / T.sizeof);
	static assert(nodeSlots >= 2);

	// default bsearch threshold is double that
	enum size_t binarySearchThreshold =
		params.binarySearchThreshold != size_t.max ? params.binarySearchThreshold : 256 / T.sizeof;

	enum bool useBinarySearch = nodeSlots > binarySearchThreshold;

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
		TreeNode node = TreeNode(true);
		alias node this;
		TreeNode*[nodeSlots+1] children;
	}
	static assert(InternalNode.init.isInternal);

 private:
	TreeNode* root = null;
	size_t totalInUse = 0;

 public:
	/// Empties the tree.
 	void clear() {
		// TODO: update this after implementing destructor = void dispose()
		this.root = null;
		this.totalInUse = 0;
	}

 	/// Implements [eris.set.ExtensionalSet.length]
	@property size_t length() const => this.totalInUse;

	/// Implements [eris.set.ExtensionalSet.at]
	inout(T)* opBinaryRight(string op : "in")(in T x) inout => search(this.root, x);

	/// Iterate over set elements.
	int opApply(scope int delegate(ref T) dg) => opApply(this.root, dg);

	/// ditto
	int opApply(scope int delegate(ref const(T)) dg) const {
		return (cast(BTree) this).opApply(cast(int delegate(ref T)) dg);
	}

	/// References an element in the set assuming it is present.
	ref inout(T) opIndex(in T x) inout in (x in this) => *(x in this);

	/// Implements [eris.set.MutableSet.upsert]
	T* upsert(in T x, scope T delegate() create, scope void delegate(ref T) update = null)
	in (create != null)
	out (p; !!(x in this) == (p != null))
	{
		// root node is lazily allocated as a leaf (at first)
		if (this.root == null) this.root = new TreeNode();

		// then insert normally (update delegate used to check for length increase)
		bool created = true;
		void delegate(ref T) update2 = (ref element){
			created = false;
			if (update != null) update(element);
		};
		TreeNode* splitNode = null;
		T* inserted = insert(this.root, x, create, update2, splitNode);
		if (created) this.totalInUse++;
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
		this.root = cast(TreeNode*) newRoot;
		return moveInserted ? &newRoot.slots[0] : inserted;
	}

	/// ditto
	T* upsert(T x) => this.upsert(x, () => move(x), (ref old){ move(x, old); });

	// TODO: bool remove(in T x, scope void delegate(ref T) destroy = null);

	bool opEquals(BTreeParameters params)(in BTree!(T, params) other) const {
		if (this.length != other.length) return false;
		foreach (ref const x; this) if (!(x in other)) return false;
		return true;
	}

	hash_t toHash() const {
		hash_t hash = 0;
		foreach (ref const x; this) hash = .hashOf(x, hash);
		return hash;
	}

 private:
	import eris.core : insertInPlaceDropLast;

	// performs a sorted search for a given needle x, returning the leftmost
	// index i such that haystack[0 .. |i|) < x <= haystack[|i| .. $); this index
	// will have its sign bit set to 1 iff x < haystack[i] (needle not found),
	// so a sign bit of 0 indicates that the needle was found at haystack[i];
	// NOTE: do NOT clear the sign bit with negation, as int.min is a possibility
	static int bisect(in T[] haystack, in T needle)
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
				const cmp = haystack[mid].opCmp(needle);
				if (cmp < 0) begin = mid + 1;
				else if (cmp == 0) return mid;
				else if (cmp > 0) end = mid;
			}
			return begin | (1 << 31);

		} else /* use linear search */ {
			int i = 0;
			for (; i < haystack.length; ++i) {
				const cmp = haystack[i].opCmp(needle);
				if (cmp < 0) continue;
				if (cmp == 0) return i;
				if (cmp > 0) break;
			}
			return i | (1 << 31);
		}
	}

	static inout(T)* search(inout(TreeNode*) node, in T key) {
		// search this node's slots (if not null)
		if (node == null) return null;
		const m = node.slotsInUse;
		int pos = bisect(node.slots[0 .. m], key);
		// on leaf nodes, we either found it or we give up
		if (pos >= 0) return &node.slots[pos];
		else if (!node.isInternal) return null;
		// but on internal nodes, we tail-recursively search a specific child
		auto internalNode = cast(inout(InternalNode*)) node;
		int child = pos & ~(1 << 31);
		return search(internalNode.children[child], key);
	}

	// in-order iteration over used slots
	static int opApply(TreeNode* node, scope int delegate(ref T) dg) {
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
	static T* split(TreeNode* left, TreeNode* right, T x, int pos) {
		return splitImpl!false(left, right, x, pos, null);
	}
	static T* split(InternalNode* left, InternalNode* right, T x, int pos, TreeNode* splitChild) {
		return splitImpl!true(cast(TreeNode*) left, cast(TreeNode*) right, x, pos, splitChild);
	}

	// split a left node's elements across it and a newly allocated right node,
	// with a pending insertion of x at index pos in the original (full) node
	pragma(inline)
	static T* splitImpl(bool isInternal)(
		TreeNode* left,
		TreeNode* right,
		T x,
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
			insertInPlaceDropLast(left.slots[0..left.slotsInUse], pos, move(x));
			static if (isInternal) insertInPlaceDropLast(leftInternal.children[0..left.slotsInUse+1], pos+1, splitChild);
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
		// a split, we know that the rightmost slot of the left node will be
		// pushed upwards to its parent, but the child pointer to the right of
		// that node needs to be put somewhere; that somewhere is precisely the
		// leftmost slot of the just-created right node, which should be null now
		static if (isInternal) {
			assert(rightInternal.children[0] == null);
			move(leftInternal.children[left.slotsInUse], rightInternal.children[0]);
		}

		return inserted;
	}

	// update an existing element or create a matching one and add it to the tree
	static T* insert(
		TreeNode* node,
		in T x,
		scope T delegate() create,
		scope void delegate(ref T) update,
		out TreeNode* splitNode,
	)
	in (node != null && create != null && update != null)
	{
		// search this (non-null) node's slots
		int pos = bisect(node.slots[0 .. node.slotsInUse], x);

		// if found, just return the right address
		if (pos >= 0) {
			update(node.slots[pos]);
			return &node.slots[pos];
		}
		pos &= ~(1 << 31);

		// when a leaf is reached and has space, insert right there
		if (!node.isInternal && node.slotsInUse < nodeSlots) {
			node.slotsInUse = node.slotsInUse + 1;
			insertInPlaceDropLast(node.slots[0..node.slotsInUse], pos, create());
			return &node.slots[pos];
		}

		// when the leaf has no slots left, we'll split it in two
		if (!node.isInternal && node.slotsInUse == nodeSlots) {
			// we'll always create a new right node, but the split is not so
			// trivial because there's still a pending insertion
			splitNode = new TreeNode();
			return split(node, splitNode, create(), pos);
		}

		// these last cases handle an internal node which doesnt't itself contain
		// x, so we know that the insertion takes place in one of its children
		auto internalNode = cast(InternalNode*) node;
		TreeNode* child = internalNode.children[pos];
		T* inserted = insert(child, x, create, update, splitNode);
		if (splitNode == null) return inserted; // child wasn't split

		// if the child was split, we'll move its median element (rightmost of
		// the left child) into this node and insert the new right child pointer
		// NOTE: what if we move the inserted element? gotta adjust the returned address
		T median = move(child.slots[child.slotsInUse - 1]);
		const bool moveInserted = &child.slots[child.slotsInUse - 1] == inserted;
		child.slotsInUse = child.slotsInUse - 1;
		pos = bisect(internalNode.slots[0..internalNode.slotsInUse], median);
		assert(pos < 0);
		pos &= ~(1 << 31);

		// which is easy when there's space in this node ...
		if (internalNode.slotsInUse < nodeSlots) {
			internalNode.slotsInUse = internalNode.slotsInUse + 1;
			insertInPlaceDropLast(internalNode.slots[0..internalNode.slotsInUse], pos, move(median));
			insertInPlaceDropLast(internalNode.children[0..internalNode.slotsInUse+1], pos+1, splitNode);
			splitNode = null;
			return moveInserted ? &internalNode.slots[pos] : inserted;
		}

		// .. but when there are no slots left, another split is needed at this level
		auto newNode = new InternalNode();
		T* splitInsert = split(internalNode, newNode, move(median), pos, splitNode);
		splitNode = cast(TreeNode*) newNode;
		return moveInserted ? splitInsert : inserted;
	}
}

unittest {
	enum BTreeParameters params = { binarySearchThreshold: 2, nodeSlots: 3 };
	alias Tree = BTree!(int, params);
	Tree btree;

	static const payload = [
		34, // -> allocate root as leaf
		33, 38,
		28, // -> root is split, becomes [33] with left=[28], right=[34,38]
		27, 22,
		30, // -> left is split (27 goes up)
		21, 24,
		18, // -> left' is split (21 goes up)
		19, 20, 26, 32, 42,
		/* at this point, the tree has two levels and is completely filled:
			r: [ (0) 21 (1) 27 (2) 33 (3) ]
			\-> 0: [ 18 19 20 ]
			\-> 1: [ 22 24 26 ]
			\-> 2: [ 28 30 32 ]
			\-> 3: [ 34 38 42 ]
		*/
		23, // -> leaf 1 is split, which overflows and splits the root as well
		/* after the overflow, we expect the tree to look something like:
			r: [ (0) 23 (1) ]
			\-> 0: [ (0) 21 (1) ]
			|	\-> 0: [ 18 19 20 ]
			|	\-> 1: [ 22 ]
			\-> 1: [ (0) 27 (1) 33 (2) ]
				\-> 0: [ 24 26 ]
				\-> 1: [ 28 30 32 ]
				\-> 2: [ 34 38 42 ]
		*/
	];

	assert(btree.length == 0);
	foreach (x; payload) {
		// at first, the set does not contain x
		assert(!(x in btree));
		// insert x and test returned address
		int* p = btree.upsert(x);
		assert(*p == x);
		// now it does contain x
		assert(x in btree);
		// lookup the element and test reference
		int* q = &(btree[x]);
		assert(p == q);
		// should not insert twice (tested using the length checked below)
		btree.upsert(x);
	}
	assert(btree.length == payload.length);
	foreach (ref const x; btree) {
		import eris.set : contains;
		assert(payload.contains(x));
	}

	version (D_BetterC) {} else {
		bool[Tree] hashtable;
		assert(!(btree in hashtable));
		hashtable[btree] = true;
		assert(btree in hashtable);
	}
}

}
