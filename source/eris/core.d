/// Core type definitions, templates and helper procedures.
module eris.core;

import core.lifetime : moveImpl = move;


/++
Zero-sized type carrying no information at all.

NOTE: Make sure not to use this in extern (C) signatures, as the ABI is unspecified.
+/
alias Unit = void[0];

static assert(Unit.sizeof == 0);


/++
Free-function version of [opCmp](https://dlang.org/spec/operatoroverloading.html#compare)
for generic code.

Can be used to efficiently compare both complex types with a custom `opCmp`, or primitive types.
+/
int opCmp(T)(in T a, in T b) {
	static if (__traits(hasMember, T, "opCmp") && !is(T == U*, U)) {
		return a.opCmp(b);
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


/++
Phobos `move`, but always by destructive copy when in debug mode.

Performs the same operations as [core.lifetime.move], but when in debug mode the
moved source is always set to its `.init` value, which is easier to check (e.g.
`null` for pointers) when looking for referential integrity bugs.
+/
void move(T)(ref T source, ref T target) {
	moveImpl(source, target);
	debug source = T.init;
}

/// ditto
T move(T)(ref T source) {
	debug scope(exit) source = T.init;
	return moveImpl(source);
}

///
nothrow @nogc pure unittest {
	int x = 1;
	int* p = &x;
	int* q = null;
	move(p, q);
	assert(q == &x);
	debug assert(p == null); // only in debug mode!
}
