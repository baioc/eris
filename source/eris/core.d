/// Core type definitions, templates and helper procedures.
module eris.core;


/// D uses `size_t` for hash values.
alias hash_t = size_t;
static assert(__traits(isUnsigned, hash_t));

/// C-style null-terminated string.
alias stringz = char*;

/++
Error code signaling; zero is success.

Rationale: Compatible with C practice (and with D's `opApply` delegate parameter).
+/
alias err_t = int;


/++
Free-function version of [opCmp](https://dlang.org/spec/operatoroverloading.html#compare);
useful to compare primitives in generic code.
+/
pragma(inline)
int opCmp(T)(in T a, in T b) if (__traits(isScalar, T)) {
	if (a < b) return -1;
	if (a > b) return 1;
	return 0;
}

///
nothrow @nogc @safe pure unittest {
	import std.meta : AliasSeq;
	static foreach (Scalar; AliasSeq!(char, ubyte, int, size_t, float, double)) {{
		const Scalar two = 2;
		const Scalar three = 3;
		assert( two.opCmp(three)  < 0 );
		assert( three.opCmp(two)  > 0 );
		assert( two.opCmp(two)   == 0 );
		assert( two.opCmp(three) != 0 );
	}}
}

nothrow @nogc @safe pure unittest {
	assert( uint.max.opCmp(1) > 0 );
	assert(  int.min.opCmp(2) < 0 );
}
