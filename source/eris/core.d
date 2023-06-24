/// Core type definitions, templates and helper procedures.
module eris.core;


/++
Zero-sized type carrying no information at all.

NOTE: Make sure not to use this in extern (C) signatures, as the ABI is unspecified.
+/
alias Unit = void[0];

static assert(Unit.sizeof == 0);


/++
Free-function version of [opCmp](https://dlang.org/spec/operatoroverloading.html#compare);
useful to efficiently compare anything (including primitives) in generic code.

NOTE: this also implements a total order over floating-point types.
+/
int opCmp(T)(in T a, in T b) {
	static if (__traits(hasMember, T, "opCmp")) {
		return a.opCmp(b);
	} else static if (__traits(isFloating, T)) {
		import std.math.operations : cmp;
		return cmp(a, b);
	} else {
		if (a < b) return -1;
		else if (a > b) return 1;
		else /* if (a == b) */ return 0;
	}
}

///
nothrow @nogc pure unittest {
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

nothrow @nogc pure unittest {
	assert( opCmp(uint.max, 1) > 0 );
	assert( opCmp(int.min, 2)  < 0 );
}

nothrow @nogc pure unittest {
	assert( opCmp(-double.nan, double.nan)       < 0 );
	assert( opCmp(-double.nan, -double.infinity) < 0 );
	assert( opCmp(double.infinity, double.nan)   < 0 );
}
