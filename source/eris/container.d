/// Static interfaces and helpers for generic containers.
module eris.container;


version (D_BetterC) {} else {
	/// Simple interface for [internal iteration via opApply](https://ddili.org/ders/d.en/foreach_opapply.html).
	interface Iterable(T) {
	 nothrow @nogc:
		/// Mutable iteration.
		int opApply(scope int delegate(ref T) nothrow @nogc dg)
		in (dg != null);

		/// Const iteration.
		int opApply(scope int delegate(ref const(T)) nothrow @nogc dg) const
		in (dg != null);
	}
}

/// Tests whether a type `I` can be used to iterate over elements of type `T`; the static counterpart of [Iterable].
template isIterable(I, T) {
	enum bool isIterable = __traits(compiles, (I it, const(I) cit) nothrow @nogc {
		foreach (ref T x; it) {}
		foreach (ref const(T) x; cit) {}
	});
}

/// ditto
template isIterable(I) {
	static if (__traits(compiles, ElementType!I))
		enum bool isIterable = isIterable!(I, ElementType!I);
	else
		enum bool isIterable = false;
}

///
nothrow @nogc @safe pure unittest {
	static assert(  isIterable!(int[]) );
	static assert( !isIterable!int     );
	static assert( !isIterable!bool    );
	static assert(  isIterable!string  );
}

version (D_BetterC) {} else {
	nothrow @nogc @safe pure unittest {
		alias T = const(string);
		alias I = Iterable!T;
		static assert(isIterable!(I, T));
		static assert(isIterable!I);
	}
}

/// Returns the first element yielded by a given iterable type (undefined if empty).
pragma(inline)
auto front(Iterable)(auto ref Iterable it) {
	foreach (x; it) return x;
	assert(0, "expected a non-empty iterable");
}

///
nothrow @nogc unittest {
	static const array = [ 1, 2, 3 ];
	assert(array.front == 1);
}

/// The element type which is iterated upon by the given [Iterable].
template ElementType(Iterable) {
	alias ElementType = typeof((){ Iterable it; return it.front; }());
}

///
nothrow @nogc @safe pure unittest {
	static assert(is(ElementType!(int[]) == int));
	static assert(is(ElementType!string : char));
}
