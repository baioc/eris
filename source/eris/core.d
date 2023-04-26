/// Core type definitions, templates and helper procedures for DasBetterC.
module eris.core;


/// D uses `size_t` for hash values.
alias hash_t = size_t;

/// C-style null-terminated string.
alias stringz = char*;

/++
Error code signaling; zero is success.

Rationale: Compatible with C (and with D's `opApply` delegate parameter).
+/
alias err_t = int;


/++
Free-function version of [opCmp](https://dlang.org/spec/operatoroverloading.html#compare);
useful to compare primitives in generic code.
+/
pragma(inline)
int opCmp(T)(in T a, in T b) if (__traits(isScalar, T)) => cast(int)(a - b);

///
unittest {
	import std.meta : AliasSeq;
	static foreach (Scalar; AliasSeq!(char, ubyte, int, size_t, float, double)) {
		{
			const Scalar two = 2;
			const Scalar three = 3;
			assert( two.opCmp(three)  < 0 );
			assert( three.opCmp(two)  > 0 );
			assert( two.opCmp(two)   == 0 );
			assert( two.opCmp(three) != 0 );
		}
	}
}


/// Free-function `empty`, which does the expected whenever `arg.length` works.
pragma(inline)
bool empty(T)(in T arg)
if (is(typeof(arg.length) : size_t) && !__traits(hasMember, T, "empty"))
=> (arg.length == 0);

///
unittest {
	struct S { size_t length; }

	S a = { length: 0 };
	assert(a.empty);

	S b  = { length: 1 };
	assert(!b.empty);
}


/++
Inserts at a given index in the slice while overwriting its last element.

Moves all elements to the right of the specified index one position forwards,
therefore overwriting the last element in the slice.
+/
void insertInPlaceDropLast(T)(T[] slice, size_t index, T x)
in (slice.length > 0 && index < slice.length)
{
	import std.algorithm.mutation : move;
	for (long i = slice.length - 1; i > index; --i) {
		move(slice[i-1], slice[i]);
	}
	move(x, slice[index]);
}

///
unittest {
	static int[] a = [ 1, 2, 4, 5 ];
	a.insertInPlaceDropLast(2, 3);
	assert(a[0] == 1); assert(a[1] == 2); assert(a[2] == 3); assert(a[3] == 4);
}