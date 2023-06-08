/// Core type definitions, templates and helper procedures.
module eris.core;


/// D uses `size_t` for hash values.
alias hash_t = size_t;
static assert(__traits(isUnsigned, hash_t));

/// C-style null-terminated string.
alias stringz = char*;

/// C-style error code; zero is success.
alias err_t = int;


/++
Free-function version of [opCmp](https://dlang.org/spec/operatoroverloading.html#compare);
useful to efficiently compare anything (including primitives) in generic code.
+/
pragma(inline)
int opCmp(T)(in T a, in T b) {
	static if (__traits(hasMember, T, "opCmp")) {
		return a.opCmp(b);
	} else {
		if (a < b) return -1;
		if (a > b) return 1;
		return 0;
	}
}

///
nothrow @nogc @safe pure unittest {
	import std.meta : AliasSeq;
	static foreach (Scalar; AliasSeq!(char, ubyte, int, size_t, float, double)) {{
		const Scalar two = 2;
		const Scalar three = 3;
		assert( opCmp(two, three)  < 0 );
		assert( opCmp(three, two)  > 0 );
		assert( opCmp(two, two)   == 0 );
		assert( opCmp(two, three) != 0 );
	}}
}

nothrow @nogc @safe pure unittest {
	assert( opCmp(uint.max, 1) > 0 );
	assert( opCmp(int.min, 2)  < 0 );
}
