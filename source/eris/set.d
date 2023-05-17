/++
Interfaces, helpers and some implementations for sets.

Please note that dynamic `interface`s are not defined in betterC mode,
only their static "concept" counterparts are.
+/
module eris.set;

import std.traits : isCallable, Unconst;
import std.range.primitives : hasLvalueElements, ElementType;


version (D_BetterC) {} else {
	/// Simplest interface for [intensionally-defined sets](https://en.wikipedia.org/wiki/Extensional_and_intensional_definitions).
	interface IntensionalSet(Element) {
		/// Tests whether an element belongs to the set.
		bool contains(in Element x) const;
	}
}

/// Tests whether a type `S` can be used as a set over elements of type `E`; the static counterpart of [IntensionalSet].
enum bool isIntensionalSet(S, E) = __traits(compiles, (const(S) s){
	bool b = s.contains(E.init);
});

version (D_BetterC) {} else {
	///
	nothrow @nogc @safe pure unittest {
		alias T = const(string);
		alias Set = IntensionalSet!T;
		static assert(isIntensionalSet!(Set, T));
	}
}

/// Allows any callable predicate to be used as an intensional set definition.
pragma(inline)
bool contains(Callable, Element)(in Callable predicate, in Element x)
if (isCallable!Callable)
{
	return predicate(x);
}

///
nothrow @nogc @safe pure unittest {
	assert( ((int n) => n % 2 == 0).contains(2) );
	const isEven = (int n) => n % 2 == 0;
	assert(!isEven.contains(1));
	static assert(isIntensionalSet!(typeof(isEven), int));
}


version (D_BetterC) {} else {
	/// A generic interface for finite sets.
	interface ExtensionalSet(Element) : IntensionalSet!Element {
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
}

/// Static interface counterpart of [ExtensionalSet].
enum bool isExtensionalSet(S, E) = __traits(compiles, (Unconst!S s, const(S) c){
	size_t size = s.length;
	foreach (ref E x;        s) E* p        = s.at(x);
	foreach (ref const(E) x; c) const(E)* q = c.at(x);
});

version (D_BetterC) {} else {
	///
	nothrow @nogc @safe pure unittest {
		alias T = const(string);
		alias Set = ExtensionalSet!T;
		static assert(isExtensionalSet!(Set, T));
		static assert(isIntensionalSet!(Set, T));
	}
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
nothrow @nogc unittest {
	// slices of type S = const T[], eg:
	alias T = int;
	static const T[] array = [ 0, 1, 1, 2, 3, 5, 8, 13 ];
	alias S = typeof(array);

	// can be seen as extensional sets over const(T):
	const(int)* p = array.at(5);
	assert(*p == 5);
	static assert(isExtensionalSet!(S, const(T)));

	// but as intensional sets, they can also range over (non-const) T:
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
nothrow @nogc unittest {
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


version (D_BetterC) {} else {
	/++
	Generic interface for mutable sets, i.e., extensional sets with deletion and
	insertion capabilities, as well as some control over preallocation.
	+/
	interface MutableSet(Element) : ExtensionalSet!Element {
		/++
		Updates an element already in the set or creates a new one therein.

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
		Simpler upsert in which value being looked up is also copied into the set.

		Params:
			x = an element which is used to look up an entry in the set;
				if the entry is not found, a copy of this value gets added to the set in
				a new entry, otherwise the existing entry is overwritten by (a copy of) this value

		Returns: A pointer to the element currently stored in the set, or `null` in case of insertion failure.
		+/
		Element* upsert(Element x)
		out (p; this.contains(x) == (p != null));

		/++
		Removes an element from the set (if it exists at all).

		Params:
			x = element being looked up in the set
			destroy = callback to cleanup after the element being removed (if found); defaults to [object.destroy](https://dlang.org/library/object/destroy.html).

		Returns: Whether or not `x` was contained in the set.
		+/
		bool remove(in Element x, scope void delegate(ref Element) destroy = null)
		out (; !this.contains(x));


		/++
		Removes all elements from the set without necessarily reducing its capacity.

		If elements are structs with a destructor defined, they will all be destroyed.
		+/
		void clear()
		out (; this.length == 0);

		/++
		Query the current (preallocated) capacity of the set, in number of elements.

		A capacity of `n` implies that the set can grow until `length == n` without insertion failures.
		If preallocation is not wanted or needed, this method can always return the set's current `length`.

		Returns: The element capacity of the set, which must be at least as big as its current `length`.
		+/
		size_t capacity() const
		out (n; n >= this.length);

		/++
		Makes an effort to increase the set's `capacity` to at least `n` elements.

		Depending on how the set is implemented and on how big `n` is, this function can fail.
		Whether it only increased capacity up to a certain point, or gave up without even trying,
		it must preserve all elements currently contained in the set.

		Params:
			n = desired capacity, must be at least as big as the number of elements currently in the set

		Returns: The currently preallocated capacity, even if it didn't increase.
		+/
		size_t reserve(size_t n)
		in (n >= this.length)
		out (c; c == this.capacity);
	}
}

/// Static interface counterpart of [MutableSet].
enum bool isMutableSet(S, E) = isExtensionalSet!(S, E) && __traits(compiles, (S s, E x){
	size_t reserved = s.capacity;
	size_t newCapacity = s.reserve(reserved << 1);
	E* p = s.upsert(x);
	E* q = s.upsert(x, () => x);
	E* r = s.upsert(x, () => x, (ref old){ old = x; });
	bool maybe = s.remove(x, (ref e){ .destroy(e); });
	bool false_ = s.remove(x);
	s.clear();
});

version (D_BetterC) {} else {
	///
	nothrow @nogc @safe pure unittest {
		alias T = int;
		alias Set = MutableSet!T;
		static assert(isMutableSet!(Set, T));
		static assert(isExtensionalSet!(Set, T));
		static assert(isIntensionalSet!(Set, T));
	}
}


// TODO: alias OrderedSet = BTree
