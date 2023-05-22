module main;

import core.stdc.string : strcmp;
import core.stdc.time : clock, clock_t, CLOCKS_PER_SEC;
import core.stdc.stdlib : rand;

import eris.core : stringz, err_t;
import eris.math : Accumulator;


version (D_Coverage) {} else extern(C) err_t main(int argc, const(stringz)* argv) {
	const(stringz)[] args = argv[0 .. argc];
	if (argc <= 1 || strcmp(args[1], "unittest") == 0) return runUnittests();
	else if (strcmp(args[1], "benchmark") == 0)        return doBenchmarks(args[2..$]);
	return __LINE__;
}

err_t runUnittests() {
	enum string[] moduleNames = [
		"eris.core",
		"eris.array",
		"eris.math",
		"eris.container",
		"eris.btree",
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

err_t doBenchmarks(const(stringz[]) args) {
	if (args.length < 2) return __LINE__;
	const type = args[0];
	import core.stdc.stdlib : atoi;
	int size = atoi(args[1]);
	if (strcmp(type, "aaset") == 0)    return benchmarkAASet(size);
	if (strcmp(type, "redblack") == 0) return benchmarkRedBlackTree(size);
	if (strcmp(type, "btree") == 0)    return benchmarkBTree(size);
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
		code(begin, end);
		double elapsedUs = (end - begin) * 1e6 / CLOCKS_PER_SEC;
		acc += elapsedUs;
	}
	return acc;
}


struct String32 {
	char[32] str;
	alias str this;
	int opCmp(ref const(String32) other) const {
		import core.stdc.string : strncmp;
		return strncmp(this.str.ptr, other.str.ptr, str.sizeof);
	}
}

void randomize(out int output, in int input = rand()) {
	output = input;
}

void randomize(out String32 output, in int input = rand()) {
	import core.stdc.stdio : snprintf;
	snprintf(output.ptr, output.length, "%032d", input);
}

err_t setBenchmarks(string name, alias Set)(int n, uint seed = 0) {
	if (n < 0) return __LINE__;
	const n2 = n * 2;

	import std.meta : AliasSeq;
	static foreach (Element; AliasSeq!(int, String32)) {{
		import core.stdc.stdlib : srand, calloc, free;

		auto buffer = cast(Element*) calloc(n2, Element.sizeof);
		if (n2 != 0 && buffer == null) return __LINE__;
		scope(exit) free(buffer);

		Element[] xs = buffer[0 .. n];
		Element[] xs2 = buffer[0 .. n2];
		srand(seed);
		Accumulator benchmarkRandomized(scope void delegate(out clock_t, out clock_t) code) {
			return (out begin, out end){
				foreach (i; 0 .. xs2.length) xs2[i].randomize();
				code(begin, end);
			}.benchmark();
		}

		import core.volatile : volatileStore;
		ulong store;

		const upsert = benchmarkRandomized((out begin, out end){
			Set!Element set;
			begin = clock();
			foreach (x; xs) set.upsert(x);
			end = clock();
		});

		const lookupFind = benchmarkRandomized((out begin, out end){
			Set!Element set;
			foreach (x; xs) set.upsert(x);
			begin = clock();
			foreach (x; xs) {
				auto b = x in set;
				volatileStore(&store, b);
			}
			end = clock();
		});

		const lookupFail = benchmarkRandomized((out begin, out end){
			Set!Element set;
			foreach (x; xs2) set.upsert(x);
			foreach (x; xs) set.remove(x);
			begin = clock();
			foreach (x; xs) {
				auto b = x in set;
				volatileStore(&store, b);
			}
			end = clock();
		});

		const remove = benchmarkRandomized((out begin, out end){
			Set!Element set;
			foreach (x; xs) set.upsert(x);
			begin = clock();
			foreach (x; xs) set.remove(x);
			end = clock();
		});

		static foreach (op; AliasSeq!(upsert, lookupFind, lookupFail, remove)) {{
			import core.stdc.stdio : printf;
			enum string format =
				"container=" ~ name ~
				"\telement=" ~ Element.stringof ~
				"\toperation=" ~ op.stringof ~
				"\tn=%d\tavg=%.3f\tstd=%.3f\tmin=%.3f\tmax=%.3f\n";
			printf(format, n, op.mean, op.std, op.min, op.max);
		}}
	}}

	return 0;
}

struct AASet(T) {
	alias Unit = void[0];
	Unit[T] aa;
	void upsert(T x) { aa[x] = Unit.init; }
	bool opBinaryRight(string op : "in")(in T x) const => (x in aa) != null;
	void remove(in T x) { aa.remove(x); }
}

err_t benchmarkAASet(int n) {
	version (D_BetterC) {
		assert(0, "no AAs in betterC mode");
	} else {
		return setBenchmarks!("AA-as-set", AASet)(n);
	}
}

struct RedBlackTree(T) {
	import std.container.rbtree;
	std.container.rbtree.RedBlackTree!T rbt = redBlackTree!T();
	void upsert(T x) { rbt.insert(x); }
	bool opBinaryRight(string op : "in")(in T x) const => x in rbt;
	void remove(in T x) { rbt.removeKey(x); }
}

err_t benchmarkRedBlackTree(int n) {
	version (D_BetterC) {
		assert(0, "no std.container.rbtree in betterC mode");
	} else {
		return setBenchmarks!("std.container.rbtree", RedBlackTree)(n);
	}
}

import eris.btree;

err_t benchmarkBTree(int n) {
	version (D_BetterC) {
		assert(0, "FIXME: no eris.set:BTree in betterC (yet)");
	} else {
		return setBenchmarks!("eris.btree", BTree)(n);
	}
}
