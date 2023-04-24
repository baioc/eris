/++
Interfaces, helpers and some implementations for sets.

Please note that dynamic `interface`s are not defined in -betterC mode,
only their static "concept" counterparts are.
+/
module eris.set;

import std.traits : isCallable, Unconst;
import std.range.primitives : hasLvalueElements, ElementType;

import eris.typecons : makeFluent;


/// Simplest interface for [intensionally-defined sets](https://en.wikipedia.org/wiki/Extensional_and_intensional_definitions).
version (D_BetterC) {} else interface IntensionalSet(Element) {
	/// Tests whether an element belongs to the set.
	bool contains(in Element x) const;
}

/// Tests whether a type `S` can be used as a set over elements of type `E`; the static counterpart of [IntensionalSet].
enum bool isIntensionalSet(S, E) = __traits(compiles, (const(S) s){
	bool b = s.contains(E.init);
});

///
version (D_BetterC) {} else unittest {
	alias T = const(string);
	alias Set = IntensionalSet!T;
	static assert(isIntensionalSet!(Set, T));
}

/// Allows any callable predicate to be used as an intensional set definition.
pragma(inline)
bool contains(Callable, Element)(in Callable predicate, in Element x)
if (isCallable!Callable)
{
	return predicate(x);
}

///
unittest {
	assert( ((int n) => n % 2 == 0).contains(2) );
	const isEven = (int n) => n % 2 == 0;
	assert(!isEven.contains(1));
	static assert(isIntensionalSet!(typeof(isEven), int));
}


/// A generic interface for finite sets.
version (D_BetterC) {} else interface ExtensionalSet(Element) : IntensionalSet!Element {
	/// Query the number of elements in the set.
	size_t length() const;

	/// Get the address of a matching element, or `null` if it isn't in the set.
	inout(Element)* at(in Element x) inout;

	/// Iterate over elements in the set using `foreach`.
	int opApply(scope int delegate(ref Element) dg)
	in (dg != null);

	/// ditto
	int opApply(scope int delegate(ref const(Element)) dg) const
	in (dg != null);
}

/// Static interface counterpart of [ExtensionalSet].
enum bool isExtensionalSet(Set, Element) = __traits(compiles, (Unconst!Set s, const(Set) c){
	size_t size = s.length;
	foreach (ref Element x;        s) Element* p        = s.at(x);
	foreach (ref const(Element) x; c) const(Element)* q = c.at(x);
});

///
version (D_BetterC) {} else unittest {
	alias T = const(string);
	alias Set = ExtensionalSet!T;
	static assert(isExtensionalSet!(Set, T));
	static assert(isIntensionalSet!(Set, T));
}

/// Gives an intensional interpretation to all extensional sets by defining `contains` in terms of `at`.
pragma(inline)
bool contains(Set, Element)(in Set s, in Element x)
if (isExtensionalSet!(const(Set), const(Element)))
{
	return s.at(x) != null;
}

/// Defines an extensional set's `at` for iterables whose elements can have their address taken (includes slices). O(n).
pragma(inline)
inout(Element)* at(Iterable, Element)(inout(Iterable) haystack, in Element needle)
if (is(Iterable == Element[]) || (hasLvalueElements!Iterable && is(ElementType!R == Element)))
{
	foreach (ref inout x; haystack) {
		if (x == needle) return &x;
	}
	return null;
}

///
unittest {
	// slices of type S = const T[], eg:
	alias T = int;
	static const T[] array = [ 0, 1, 1, 2, 3, 5, 8, 13 ];
	alias S = typeof(array);

	// can be seen as extensional sets over const(T):
	const(int)* p = array.at(5);
	assert(*p == 5);
	static assert(isExtensionalSet!(S, const(T)));

	// but as itensional sets, they can also range over (non-const) T:
	assert(array.contains(1));
	assert(!array.contains(4));
	static assert(isIntensionalSet!(S, T));
	static assert(isIntensionalSet!(S, const(T)));
}

/++
Copies a matching element from the set, or returns a given default value.

Params:
	set = an extensional set
	x = element to look for in the set
	default_ = default value returned when no element matching `x` is found
+/
pragma(inline)
inout(Element) get(Set, Element)(auto ref inout(Set) set, in Element x, inout(Element) default_)
if (isExtensionalSet!(Set, Element))
{
	inout(Element)* entry = set.at(x);
	return entry ? *entry : default_;
}

///
unittest {
	static const notes = [
		"C", "C#", "Db", "D", "D#", "Eb", "E",
		"F", "F#", "Gb", "G", "G#", "Ab", "A", "A#", "Bb", "B"
	];
	auto d = notes.get("D", "not found");
	assert(d == "D");
	auto notFound = "not found";
	auto es = notes.get("E#", notFound);
	assert(es == notFound);
}


/++
Generic interface for mutable sets, i.e., extensional sets with deletion and
insertion capabilities, as well as some control over preallocation.
+/
version (D_BetterC) {} else interface MutableSet(Element) : ExtensionalSet!Element {
	/++
	Looks up an element in the set, updates it if it exists and inserts a new one otherwise.

	Params:
		x = element being looked up in the set
		create = callback to create a new matching element, which is then added to the set
		update = callback to modify an existing element in the set; only called if not `null`

	Returns:
		A pointer to the element currently stored in the set, whether it was updated or inserted.
		This value is only `null` if the element would have been inserted but
		the operation didn't succeed (e.g. because of a memory allocation failure).
	+/
	Element* upsert(in Element x, scope Element delegate() create, scope void delegate(ref Element) update = null)
	in (create != null)
	out (p; this.contains(x) == (p != null));

	/++
	Simpler upsert in which the same value being looked up is also upserted into the set.

	Params:
		x = an element which is used to look up an entry in the set;
			if the entry is not found, a copy of this value gets added to the set in
			a new entry, otherwise the existing entry is overwritten by this value.

	Returns: A pointer to the element currently stored in the set, or `null` in case of insertion failure.
	+/
	Element* upsert(Element x)
	out (p; this.contains(x) == (p != null));

	/++
	Removes an element from the set (if it exists at all).

	Params:
		x = element being looked up in the set
		destroy = callback to cleanup after the element being removed (if found); only called if not `null`

	Returns: Whether or not `x` was contained in the set.
	+/
	bool remove(in Element x, scope void delegate(ref Element) destroy = null)
	out (; !this.contains(x));

	/// Empties the set.
	void clear()
	out (; this.length == 0);

	/++
	Query the current (preallocated) capacity of the set, in number of elements.

	A capacity of `n` implies that the set can grow until `length == n` without insertion failures.
	If preallocation is not wanted or needed, this method can always return the set's current `length`.

	Returns: The element capacity of the set, which must be at least as big as its current `length`.

	See_Also: upsert
	+/
	size_t capacity() const
	out (n; n >= this.length);

	/++
	Makes an effort to increase the set's `capacity` to at least `n` elements.

	Depending on how the set is implemented and on how big `n` is, this function can fail.
	Whether it only increased capacity up to a certain point, or gave up without even trying,
	it must preserve existing elements currently contained in the set.

	Params:
		n = desired capacity, must be at least as big as the number of elements currently in the set

	Returns: The currently preallocated capacity, even if it didn't increase.
	+/
	size_t reserve(size_t n)
	in (n >= this.length)
	out (c; c == this.capacity);
}

/// Static interface counterpart of [MutableSet].
enum bool isMutableSet(Set, Element) = isExtensionalSet!(Set, Element) && __traits(compiles, (Set s, Element x){
	size_t reserved = s.capacity;
	size_t newCapacity = s.reserve(reserved << 1);
	Element* p = s.upsert(x);
	Element* q = s.upsert(x, () => x);
	Element* r = s.upsert(x, () => x, (ref old){ old = x; });
	bool maybe = s.remove(x, (ref e){ .destroy(e); });
	bool false_ = s.remove(x);
	s.clear();
});

///
version (D_BetterC) {} else unittest {
	alias T = int;
	alias Set = MutableSet!T;
	static assert(isMutableSet!(Set, T));
	static assert(isExtensionalSet!(Set, T));
	static assert(isIntensionalSet!(Set, T));
}

/// Set wrapper around D's [associative arrays](https://dlang.org/spec/hash-map.html) (not betterC-compatible).
version (D_BetterC) {} else struct AASet(Element) {
	// unfortunately, AAs don't give us access their keys' addresses, so we would
	// need to split Element into Key+Value and store an AA of type Element[Key],
	// but this leads to duplicating Key memory and the split can't be automated.
	// this is supposed to be a set and not a map, so I want to keep a single type
	// parameter. this leads us to the next strategy: refs to heap-allocated memory
 private:
	import eris.core : hash_t;

	struct HashableRef {
		Element* ptr;
		invariant (ptr != null);
		@disable this();
		inout this(inout(Element)* ptr) { this.ptr = ptr; }
	@safe pure nothrow:
		hash_t toHash() const => .hashOf(*ptr);
		bool opEquals(const(HashableRef) other) const {
			return this.ptr == other.ptr || *this.ptr == *other.ptr;
		}
	}

	HashableRef[const(HashableRef)] aa;

 public:
	///
	this(Element[] elements) {
		foreach (ref x; elements) this.upsert(x);
	}

	string toString() const {
		import std.array : appender;
		import std.conv : to;

		if (this.length == 0) return "#{}";

		auto result = appender!string;
		result ~= "#{ ";

		bool first = true;
		foreach (ref x; this) {
			if (!first) {
				result ~= ", ";
			} else {
				first = false;
			}
			result ~= x.to!string;
		}

		result ~= " }";
		return result[];
	}

	@safe pure nothrow {
		bool opEquals(in AASet other) const => this.aa == other.aa;
		hash_t toHash() const => .hashOf(aa);
	}

	bool contains(in Element x) const => this.at(x) != null;

	size_t length() const => aa.length;
	inout(Element)* at(in Element x) inout {
		auto key = const(HashableRef)(&x);
		auto hrp = key in aa;
		return hrp == null ? null : hrp.ptr;
	}
	int opApply(scope int delegate(ref Element) dg) {
		foreach (hr; aa) {
			int stop = dg(*hr.ptr);
			if (stop) return stop;
		}
		return 0;
	}
	int opApply(scope int delegate(ref const(Element)) dg) const {
		return (cast(AASet) this).opApply(cast(int delegate(ref Element)) dg);
	}

 	Element* upsert(Element x) {
		// copy a struct on the heap and get the pointer to it:
		// https://forum.dlang.org/thread/j9m091$1r5g$1@digitalmars.com
		Element* heapCopy = [x].ptr;
		auto key = const(HashableRef)(heapCopy);
		auto value = HashableRef(heapCopy);
		aa[key] = value;
		return heapCopy;
	}
	Element* upsert(in Element x, scope Element delegate() create, scope void delegate(ref Element) update = null) {
		// this one's a bit tricky: AAs set the key before the create delegate
		// gets called, so we better give it a heap-allocated key; however that
		// allocation is unnecessary for the update case. now, we'll either have
		// to do 1 extra allocation, or 1 extra lookup ... so lookup it is
		Element* current = this.at(x); // <-- 1st lookup
		if (current != null) {
			if (update != null) update(*current);
			return current;
		}
		// at this point, we know its a standard insert
		return this.upsert(create()); // <-- does a 2nd lookup
	}
	bool remove(in Element x, scope void delegate(ref Element) destroy = null) {
		auto key = const(HashableRef)(&x);
		if (destroy == null) return aa.remove(key);
		auto hrp = key in aa;
		if (hrp == null) return false;
		aa.remove(key);
		destroy(*hrp.ptr);
		return true;
	}
	void clear() => aa.clear();
	size_t capacity() const => this.length;
	size_t reserve(size_t n) => this.capacity;
}

version (D_BetterC) {} else {
	/// Convenience function for creating an AASet from a list of values.
	pragma(inline) auto aaSet(T)(T[] elems...) => AASet!T(elems);

	///
	unittest {
		auto set = aaSet("ALGOL", "BCPL", "B", "C", "D");
		const complete = aaSet("ALGOL", "BCPL", "B", "C", "C++", "Java", "D");
		static assert(isMutableSet!(typeof(set), string));

		assert(set.length == 5);
		assert(set.contains("D"));
		assert(!set.contains("C++"));
		assert(set != complete);

		assert(set.reserve(7) >= 2);
		foreach (x; ["C", "C++", "Java"]) set.upsert(x, () => x);
		assert(set.length == 7);
		assert(set.contains("C++"));
		assert(set == complete);
		string serialized = set.toString;
		foreach (const s; complete) {
			import std.algorithm.searching : canFind;
			assert(serialized.canFind(s));
		}

		assert(set.remove("Java", (ref removed){}));
		assert(!set.remove("Java"));
		assert(set.length == 6);

		set.clear();
		assert(set.length == 0);
		assert(!set.contains("D"));
		assert(!set.contains("C"));
	}
}


// TODO: make this betterC-compatiable
// TODO: destructor = void dispose()
// TODO: operators (toHash, opEquals, opApply, etc)
version (D_BetterC) {} else {

/// Static parameters for [BTree] template. Use `size_t.max` for defaults.
struct BTreeParameters {
	/// Maximum number of elements stored per node, must be at least 2.
	size_t nodeSlots = size_t.max;

	/// If `nodeSlots` is greater than this, the B-Tree will use a binary intra-node search method.
	size_t binarySearchThreshold = size_t.max;
}

mixin makeFluent!BTreeParameters;

/// An ordered set ADT backed by a B-Tree.
struct BTree(T, BTreeParameters params = BTreeParameters.init) {
 private:
	import std.algorithm.comparison : max;
	import std.algorithm.mutation : move;

	// since nodeSlots consist of a contiguous array of Ts, we can choose a
	// default based on L2 "Streamer" prefetch block size (128 B)
	enum size_t nodeSlots =
		params.nodeSlots != size_t.max ? params.nodeSlots : max(2, 128 / T.sizeof);
	static assert(nodeSlots >= 2);

	// default bsearch threshold is similar, but since it applies to internal
	// nodes as well, we'll double it to account for pointer overhead
	enum size_t binarySearchThreshold =
		params.binarySearchThreshold != size_t.max ? params.binarySearchThreshold : 256 / T.sizeof;

	enum bool useBinarySearch = nodeSlots > binarySearchThreshold;

 private:
	struct TreeNode {
		import std.bitmanip : bitfields;
		mixin(bitfields!(
			uint, "slotsInUse", 31,
			bool, "isInternal", 1)
		);
		T[nodeSlots] slots;
		this(bool isInternal) { this.isInternal = isInternal; }
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

 public: // ExtensionalSet
	size_t length() const => this.totalInUse;
	inout(T)* at(in T x) inout => search(this.root, x);
	int opApply(scope int delegate(ref T) dg) => opApply(this.root, dg);
	int opApply(scope int delegate(ref const(T)) dg) const {
		return (cast(BTree) this).opApply(cast(int delegate(ref T)) dg);
	}

 public: // TODO MutableSet
	T* upsert(in T x, scope T delegate() create, scope void delegate(ref T) update = null) {
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

	T* upsert(T x) => this.upsert(x, () => move(x), (ref old){ move(x, old); });

	// bool remove(in T x, scope void delegate(ref T) destroy = null);
	// void clear();
	// size_t capacity() const;
	// size_t reserve(size_t n);

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

	// split a left node's elements across it and a newly allocated right node,
	// with a pending insertion of x at index pos in the original (full) node
	pragma(inline)
	static T* split(bool isInternal = false)(
		TreeNode* left,
		TreeNode* right,
		T x,
		int pos,
		TreeNode* splitChild = null
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
		T* splitInsert = split!true(cast(TreeNode*) internalNode, cast(TreeNode*) newNode, move(median), pos, splitNode);
		splitNode = cast(TreeNode*) newNode;
		return moveInserted ? splitInsert : inserted;
	}
}

unittest {
	alias Tree = BTree!(int, BTreeParameters()
		.withBinarySearchThreshold(2)
		.withNodeSlots(3)
	);
	Tree btree;

	static const payload = [
		34, // -> allocate root as leaf
		28, 38,
		33, // -> root is split, becomes [33] with left=[28], right=[34,38]
		27, 22,
		30, // -> left is split (27 goes up)
		18, 24,
		21, // -> left' is split (21 goes up)
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
		assert(!btree.contains(x));
		// insert x and test returned address
		int* p = btree.upsert(x);
		assert(*p == x);
		// now it does contain x
		assert(btree.contains(x));
		// lookup the element and test reference
		int* q = btree.at(x);
		assert(p == q);
		// should not insert twice (tested using the length checked below)
		btree.upsert(x);
	}
	assert(btree.length == payload.length);
	foreach (ref const x; btree) assert(payload.contains(x));
}

}
