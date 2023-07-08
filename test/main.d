module main;

import core.stdc.string : strcmp;
import core.stdc.time : clock, clock_t, CLOCKS_PER_SEC;
import core.stdc.stdlib : rand, srand;

import eris.math : Accumulator;

alias stringz = char*;


version (D_Coverage) {} else extern(C) int main(int argc, const(stringz)* argv) {
	const(stringz)[] args = argv[0 .. argc];
	if (argc <= 1 || strcmp(args[1], "unittest") == 0) return runUnittests();
	else if (strcmp(args[1], "benchmark") == 0)        return doBenchmarks(args[2..$]);
	return __LINE__;
}

int runUnittests() {
	enum string[] moduleNames = [
		"eris.core",
		"eris.array",
		"eris.math",
		"eris.allocator",
		"eris.btree",
		"eris.container",
		"eris.set",
	];
	static foreach (moduleName; moduleNames) {
		mixin(`import ` ~ moduleName ~ `;`);
		static foreach (unitTest; __traits(getUnitTests, mixin(moduleName))) {
			unitTest();
		}
	}
	return 0;
}

int doBenchmarks(const(stringz[]) args) {
	import core.stdc.stdlib : atoi;
	if (args.length < 3) return __LINE__;
	const container = args[0];
	const element = args[1];
	int n = atoi(args[2]);
	if (strcmp(container, "aaset") == 0)    return benchmarkAASet(element, n);
	if (strcmp(container, "redblack") == 0) return benchmarkRedBlackTree(element, n);
	if (strcmp(container, "btree") == 0)    return benchmarkBTree(element, n);
	return __LINE__;
}


Accumulator benchmark(
	scope void delegate(out clock_t, out clock_t) code,
	uint replications = 30
)
in (code != null)
{
	Accumulator acc;
	foreach (rep; 0 .. replications) {
		clock_t begin, end;
		version (D_BetterC) {} else {
			import core.memory : GC;
			GC.collect();
		}
		code(begin, end);
		double elapsedUs = (end - begin) * 1e6 / CLOCKS_PER_SEC;
		acc += elapsedUs;
	}
	return acc;
}

nothrow @nogc pure
size_t fnv(const(ubyte)[] bytes) {
	uint hash = 2166136261u;
	foreach (b; bytes) {
		hash ^= b;
		hash *= 16777619u;
	}
	return hash;
}

struct String32 {
	char[32] str;
	alias str this;
 nothrow @nogc:
	int opCmp(in String32 other) const {
		import core.stdc.string : strncmp;
		return strncmp(this.str.ptr, other.str.ptr, str.sizeof);
	}
	size_t toHash() const @trusted {
		return fnv((cast(const(ubyte)*)this.str)[0 .. str.sizeof]);
	}
	bool opEquals(in String32 other) const => (this.opCmp(other) == 0);
}

void randomize(int* output) {
	*output = rand();
}

void randomize(String32* output) {
	import core.stdc.stdio : snprintf;
	snprintf(output.ptr, output.length, "%031d", rand());
}

int setBenchmarks(string name, alias Set, Element)(int n, uint seed = 0) {
	if (n < 0) return __LINE__;
	const n2 = n * 2;

	import core.stdc.stdlib : calloc, free, exit;
	auto buffer = cast(Element*) calloc(n2, Element.sizeof);
	scope(exit) free(buffer);
	if (n2 != 0 && buffer == null) return __LINE__;

	srand(seed);
	foreach (i; 0 .. n2) randomize(&buffer[i]);
	Element[] xs = buffer[0 .. n];
	Element[] xs2 = buffer[0 .. n2];

	const upsert = benchmark((out begin, out end){
		auto set = Set!Element();
		begin = clock();
		foreach (ref x; xs) set.upsert(x);
		end = clock();
	});

	const lookupFind = benchmark((out begin, out end){
		auto set = Set!Element();
		foreach (ref x; xs) set.upsert(x);
		begin = clock();
		foreach (ref x; xs) {
			if (x !in set) exit(__LINE__);
		}
		end = clock();
	});

	import std.algorithm.comparison : max;
	int rss = 0;
	const lookupFail = benchmark((out begin, out end){
		auto set = Set!Element();
		foreach (ref x; xs2) set.upsert(x);
		rss = max(rss, cast(int) set.length);
		foreach (ref x; xs) set.remove(x);
		begin = clock();
		foreach (ref x; xs) {
			if (x in set) exit(__LINE__);
		}
		end = clock();
	});

	const remove = benchmark((out begin, out end){
		auto set = Set!Element();
		foreach (ref x; xs) set.upsert(x);
		begin = clock();
		foreach (ref x; xs) set.remove(x);
		end = clock();
	});

	import std.meta : AliasSeq;
	static foreach (op; AliasSeq!(upsert, lookupFind, lookupFail, remove)) {{
		import core.stdc.stdio : printf;
		enum string format =
			"container=" ~ name ~
			"\telement=" ~ Element.stringof ~
			"\toperation=" ~ op.stringof ~
			"\tn=%d\tres=%d\tavg=%.3f\tstd=%.3f\tmin=%.3f\tmax=%.3f\n";
		printf(format, n, rss, op.mean, op.std, op.min, op.max);
	}}

	return 0;
}


struct AASet(T) {
	import eris.core : Unit;
	Unit[T] aa;
	void upsert(T x) { aa[x] = Unit.init; }
	bool opBinaryRight(string op : "in")(in T x) const => (x in aa) != null;
	void remove(in T x) { aa.remove(x); }
	@property size_t length() const => aa.length;
}

int benchmarkAASet(const(stringz) element, int n) {
	version (D_BetterC) {
		assert(0, "no AAs in betterC mode");
	} else {
		if (strcmp(element, "int") == 0) {
			return setBenchmarks!("AA-as-set", AASet, int)(n);
		} else if (strcmp(element, "String32") == 0) {
			return setBenchmarks!("AA-as-set", AASet, String32)(n);
		} else {
			return __LINE__;
		}
	}
}

struct RedBlackTree(T) {
	import std.container.rbtree;
	std.container.rbtree.RedBlackTree!T rbt;
	static RedBlackTree opCall() { RedBlackTree t = { rbt: redBlackTree!T() }; return t; }
	void upsert(T x) { rbt.insert(x); }
	bool opBinaryRight(string op : "in")(in T x) const => x in rbt;
	void remove(in T x) { rbt.removeKey(x); }
	@property size_t length() const => rbt.length;
}

int benchmarkRedBlackTree(const(stringz) element, int n) {
	version (D_BetterC) {
		assert(0, "no std.container.rbtree in betterC mode");
	} else {
		if (strcmp(element, "int") == 0) {
			return setBenchmarks!("std.container.rbtree", RedBlackTree, int)(n);
		} else if (strcmp(element, "String32") == 0) {
			return setBenchmarks!("std.container.rbtree", RedBlackTree, String32)(n);
		} else {
			return __LINE__;
		}
	}
}

struct BTree(T) {
	import eris.btree;
	eris.btree.BTree!T btree;
	~this() { btree.clear(); }
	@disable this(this);
	void upsert(T x) { btree.put(x); }
	bool opBinaryRight(string op : "in")(in T x) const => x in btree;
	void remove(in T x) { btree.remove(x); }
	@property size_t length() const => btree.length;
}

int benchmarkBTree(const(stringz) element, int n) {
	if (strcmp(element, "int") == 0) {
		return setBenchmarks!("eris.btree", BTree, int)(n);
	} else if (strcmp(element, "String32") == 0) {
		return setBenchmarks!("eris.btree", BTree, String32)(n);
	} else {
		return __LINE__;
	}
}
