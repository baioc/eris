/// In-place manipulation of slices.
module eris.array;

import eris.core : move;


/++
Shift all elements in the slice by `n > 0` positions to the right.

The last few `n` elements in the slice will get overwritten.

See_Also: [shift]
+/
void shiftRight(T)(T[] slice, size_t n = 1)
in (n > 0 && slice.length >= n)
{
	const long step = n;
	for (long i = slice.length - 1u; i - step >= 0; --i) {
		move(slice[i - step], slice[i]);
	}
}

/++
Inserts at a given index in the slice while dropping its last element.

Basically a macro for
----
slice[index .. $].shiftRight(1);
slice[index] = x;
----
+/
void shift(T)(T[] slice, size_t index, auto ref T x) {
	slice[index .. $].shiftRight();
	move(x, slice[index]);
}

///
nothrow @nogc unittest {
	static int[] a = [ 1, 2, 4, 5 ];
	a.shift(2, 3);
	assert(a[0] == 1); assert(a[1] == 2); assert(a[2] == 3); assert(a[3] == 4);
}

nothrow @nogc unittest {
	static int[] justOne = [ 1 ];
	justOne.shift(0, -1);
	assert(justOne[0] == -1);
}

/++
Shift elements in the slice by `n > 0` positions to the left.

The first few `n` elements in the slice will get overwritten.

See_Also: [unshift]
+/
void shiftLeft(T)(T[] slice, size_t n = 1)
in (n > 0)
{
	for (long i = 0; i + n < slice.length; ++i) {
		move(slice[i + n], slice[i]);
	}
}

///
nothrow @nogc unittest {
	static int[] empty = [];
	empty.shiftLeft(2); // does nothing
}

/++
Pops an elemens from a given index in the slice.

Basically a macro for
----
T temp = slice[index];
slice[index .. $].shiftLeft(1);
return temp;
----
+/
T unshift(T)(T[] slice, size_t index)
in (index < slice.length)
{
	T temp = move(slice[index]);
	slice[index .. $].shiftLeft();
	return temp;
}

///
nothrow @nogc unittest {
	static int[] a = [ 1, 2, 3, 4 ];
	int extracted = a.unshift(2);
	assert(extracted == 3);
	assert(a[0] == 1); assert(a[1] == 2); assert(a[2] == 4);
}

nothrow @nogc unittest {
	static int[] justOne = [ 1 ];
	int one = justOne.unshift(0);
	assert(1 == one);
}
