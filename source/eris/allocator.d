/++
Custom memory allocators and related helpers.

Based on [std.experimental.allocator](https://dlang.org/phobos/std_experimental_allocator.html),
without being strictly compatible.
+/
module eris.allocator;

import std.math.traits : isPowerOf2;
import std.traits : hasElaborateDestructor;
import core.lifetime : emplace;
import std.algorithm.comparison : max;


version (D_BetterC) {} else {
	/++
	Base allocator interface.

	All memory obtained with `allocate` must be disposed by `deallocate` on the same allocator instance.

	NOTE: Unlike [std.experimental.allocator](https://dlang.org/phobos/std_experimental_allocator_building_blocks.html),
	we always require a deallocation procedure.
	+/
	interface Allocator {
	 nothrow:
		/++
		Attempts to allocate a memory block of `size` bytes.

		If `size == 0`, the returned slice might have a null `.ptr`, but either way
		it can be safely [deallocate]d.

		Returns: The allocated memory block, or null on allocation failure.
		+/
		void[] allocate(size_t size) out (mem; mem.ptr == null || mem.length == size);

		/++
		Deallocates a previously-allocated memory block.

		NOTE: Unlike in std.experimental.allocator, this method does not have to return anything.
		+/
		void deallocate(void[] memory);

		/// Minimum alignment of all allocated memory blocks.
		@property uint alignment() const out (a; a > 0 && a.isPowerOf2);
	}
}

/// Static interface counterpart of [Allocator].
enum bool isAllocator(T) = __traits(compiles, (T a, const(T) c) nothrow {
	void[] mem = a.allocate(size_t.init);
	a.deallocate(mem);
	uint alignment = c.alignment;
});

version (D_BetterC) {} else {
	///
	nothrow @nogc pure unittest {
		static assert(isAllocator!Allocator); // obviously
		static assert(isAllocator!Mallocator);
	}
}


/// Allocate and construct an object of the specified type.
T* make(T, Allocator, Args...)(ref Allocator alloc, auto ref Args args)
if (isAllocator!Allocator)
{
	void[] mem = alloc.allocate(T.sizeof);
	if (mem.ptr == null) return null;
	return emplace!T(mem, args);
}

///
nothrow @nogc unittest {
	struct S { bool ok = false; bool notOk = true; }
	Mallocator alloc;
	S* p = alloc.make!S(true, false);
	scope(exit) alloc.dispose(p);
	assert(p.ok);
	assert(!p.notOk);
}

/// Make an array of the specified type.
T[] makeArray(T, Allocator)(ref Allocator alloc, size_t length, T init = T.init)
if (isAllocator!Allocator)
{
	import core.checkedint : mulu;

	bool overflow = false;
	size_t bytes = mulu(T.sizeof, length, overflow);
	if (overflow) return null;

	void[] mem = alloc.allocate(bytes);
	if (mem.ptr == null) return null;
	auto array = cast(T[]) mem;

	for (size_t i = 0; i < length; ++i) emplace(&array[i], init);
	return array;
}

///
nothrow @nogc unittest {
	Mallocator alloc;
	ubyte[] mem = alloc.makeArray!ubyte(64);
	scope(exit) alloc.dispose(mem);
}

/// Destroy and deallocate a given object/array.
void dispose(Allocator, T)(ref Allocator alloc, auto ref T* p)
if (isAllocator!Allocator)
{
	static if (hasElaborateDestructor!T) {
		if (p) destroy(*p);
	}
	void[] mem = (cast(void*)p)[0 .. T.sizeof];
	alloc.deallocate(mem);
	debug p = null; // when byref, this should help detect use-after-free
}

/// ditto
void dispose(Allocator, T)(ref Allocator alloc, auto ref T[] array)
if (isAllocator!Allocator)
{
	static if (hasElaborateDestructor!T) {
		foreach (ref x; array) destroy(x);
	}
	size_t bytes = array.length * T.sizeof;
	assert(T.sizeof != 0 || (bytes / T.sizeof == array.length));
	// ^ assert no overflow because we checked during makeArray
	void[] mem = (cast(void*)array.ptr)[0 .. bytes];
	alloc.deallocate(mem);
	debug array = null; // ditto the other overload
}


/// Alignment large enough for any scalar type.
enum uint platformAlignment = max(
	long.alignof, real.alignof, (void*).alignof, (void delegate()).alignof
);


/// C stdlib allocator using malloc and free.
struct Mallocator {
	private import core.stdc.stdlib : calloc, free;

 pragma(inline) nothrow @nogc:
	void[] allocate(size_t size)
	out (mem; mem.ptr == null || mem.length == size)
	{
		void* p = calloc(1, size);
		if (p == null) return null;
		else return p[0 .. size];
	}

	void deallocate(void[] memory) => free(memory.ptr);

	enum uint alignment = platformAlignment;
}
