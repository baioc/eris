/++
A non-standard library for [DasBetterC](https://dlang.org/spec/betterc.html).

This is the library I wish I had when I started using D as a better alternative and complement to C.


Goals_:

The following list ranks the goals of this project, in order of importance:
$(NUMBERED_LIST
	* Maintain compatibility with `-betterC` mode and/or[1] `nothrow @nogc`.
	* Maintain portability across mainstream platforms (x86-64 for desktop, ARM for mobile and WASM for the web).
	* Provide good documentation for all publicly exposed functionality.
	* Provide fast single-threaded implementations of certain [ADTs](https://en.wikipedia.org/wiki/Abstract_data_type) and algorithms.
)

Rationale for the first point is that it allows C developers to use D-generated code in their apps and libraries,
even if that may require some explicit template instantations, manual symbol mangling directives and `extern(C)`.
The other goals constitute reasons for wanting to use this library in C or D codebases in the first place.

$(DIV $(ID note-1)
	[1] BetterC mode works best with LDC.
	In fact, using `-betterC` with DMD often leads to impossible-to-debug linker errors.
	Therefore, betterC compatibility is only tested against LDC.
	Still, `@nogc` and `nothrow` should apply for all compilers.
)


Dependencies_:

Since I still value my time and sanity, I gave up on trying to partition this library into DUB subpackages.
So, if you want to use any particular module here without the rest of the project,
just make sure you carry its dependency subtree along with it.
Here's a dependency graph (forest) to help with that (gray nodes aren't `public import`ed in the `eris` module):

![intra-package dependencies](./deps.svg)


Note_:

$(LIST
	* D `interface`s showing up in the docs are only there to document static interfaces;
	this is a betterC library and procedures assume all containers are `struct`s,
	so classes probably won't work in most cases.
)


Link_References:
	1 = [#note-1]
+/
module eris;

public import eris.core;
public import eris.math;
public import eris.array;
public import eris.container;
public import eris.allocator;
public import eris.set;
