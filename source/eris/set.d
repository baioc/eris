/++
Interfaces, helpers and some implementations for sets.

Please note that dynamic `interface`s are not defined in -betterC mode,
only their static "concept" counterparts are.
+/
module eris.set;

import std.traits : isCallable, Unconst;
import std.range.primitives : hasLvalueElements, ElementType;


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
	int opApply(scope int delegate(ref const(Element)) dg) const
	in (dg != null);

	/// ditto
	int opApply(scope int delegate(ref Element) dg)
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

/// Set wrapper around D's [associative arrays](https://dlang.org/spec/hash-map.html).
version (D_BetterC) {} else class AASet(Element) : MutableSet!Element {
	// unfortunately, AAs don't give us access their keys' addresses, so we would
	// need to split Element into Key+Value and store an AA of type Element[Key],
	// but this leads to duplicating Key memory and the split can't be automated.
	// this is supposed to be a set and not a map, so I want to keep a single type
	// parameter. this leads us to the next strategy: refs to heap-allocated memory
 private:
	struct HashableRef {
		Element* ptr;
		invariant (ptr != null);
		@disable this();
		inout this(inout(Element)* ptr) { this.ptr = ptr; }
	@safe pure nothrow:
		size_t toHash() const { return .hashOf(*ptr); }
		bool opEquals(const(HashableRef) other) const {
			return this.ptr == other.ptr || *this.ptr == *other.ptr;
		}
	}

	HashableRef[const(HashableRef)] aa;

 public:
	///
	this(Element[] elements = []) {
		foreach (ref x; elements) this.upsert(x);
	}

 override:
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

	bool opEquals(Object obj) {
		auto other = cast(AASet) obj;
		if (other is null) return false;
		return this.aa == other.aa;
	}
	size_t toHash() @safe nothrow { return .hashOf(aa); }

 override:
	bool contains(in Element x) const { return this.at(x) != null; }

 override:
	size_t length() const { return aa.length; }
	inout(Element)* at(in Element x) inout {
		auto key = const(HashableRef)(&x);
		auto hrp = key in aa;
		return hrp == null ? null : hrp.ptr;
	}
	int opApply(scope int delegate(ref const(Element)) dg) const {
		return (cast(AASet)this).opApply(cast(int delegate(ref Element)) dg);
	}
	int opApply(scope int delegate(ref Element) dg) {
		foreach (hr; aa) {
			int stop = dg(*hr.ptr);
			if (stop) return stop;
		}
		return 0;
	}

 override:
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
	void clear() { aa.clear(); }
	size_t capacity() const { return this.length; }
	size_t reserve(size_t n) { return this.capacity; }
}

version (D_BetterC) {} else {
	/// Convenience function for creating an AASet from a list of values.
	AASet!T aaSet(T)(T[] elems...) {
		return new AASet!T(elems);
	}

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
