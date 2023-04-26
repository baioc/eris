module main;

import core.stdc.string : strcmp;
import core.stdc.stdlib : atoi, rand, srand, malloc, free;
import core.stdc.time : clock, clock_t, CLOCKS_PER_SEC;
import core.stdc.stdio : printf;

import eris.core : stringz, err_t;


version (D_Coverage) {} else extern(C) err_t main(int argc, const(stringz)* argv) {
	const(stringz)[] args = argv[0 .. argc];
	if (argc <= 1 || strcmp(args[1], "unittest") == 0) return unittests();
	else if (strcmp(args[1], "benchmark") == 0)        return benchmark(args[2..$]);
	return 1;
}

err_t unittests() {
	enum string[] moduleNames = [
		"eris.core",
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


err_t benchmark(const(stringz[]) args) {
	const n = args.length;
	if (n < 1) return 1;
	const type = args[0];
	if (strcmp(type, "aaset") == 0 && n >= 3)  return benchmarkAASet(atoi(args[1]), args[2][0]);
	if (strcmp(type, "redblack") == 0 && n >= 3) return benchmarkRedBlackTree(atoi(args[1]), args[2][0]);
	if (strcmp(type, "btree") == 0 && n >= 3) return benchmarkBTree(atoi(args[1]), args[2][0]);
	return 1;
}

double microBenchMark(
	scope void delegate(const(int[]), out clock_t, out clock_t) code,
	uint inputSize, uint seed = 0
) {
	int* rands = cast(int*) malloc(inputSize * int.sizeof);
	if (inputSize != 0 && rands == null) return double.nan;
	scope(exit) free(rands);

	srand(seed);
	foreach (i; 0 .. inputSize) rands[i] = rand();

	int[] slice = rands[0 .. inputSize];
	clock_t begin, end;
	code(slice, begin, end);

	double elapsedMs = (end - begin) * 1e3 / CLOCKS_PER_SEC;
	return elapsedMs;
}


err_t benchmarkUpsertReadRemove(string name, string init, string upsert, string read, string remove)(
	int n, char mode
) {
	if (n < 0) return 1;
	double elapsedMs = (const(int[]) inputs, out begin, out end){
		mixin(init);
		switch (mode) {
			case 'w': // only measure insertions
				begin = clock();
				foreach (x; inputs) { mixin(upsert); }
				end = clock();
				break;
			case 'r': // only measure reads
				foreach (x; inputs) { mixin(upsert); }
				begin = clock();
				foreach (x; inputs) { mixin(read); }
				end = clock();
				break;
			case 'x': // measure insert+read+remove
				begin = clock();
				foreach (x; inputs) { mixin(upsert); }
				foreach (x; inputs) { mixin(read); }
				foreach (x; inputs) { mixin(remove); }
				end = clock();
				break;
			default: assert(0);
		}
	}.microBenchMark(n);
	printf("%.*s: %.3f ns/element\n", cast(int) name.length, name.ptr, elapsedMs * 1e6 / n);
	return 0;
}

err_t benchmarkAASet(int n, char mode) {
	version (D_BetterC) {
		assert(0, "no AAs in betterC mode");
	} else {
		return benchmarkUpsertReadRemove!("AA-as-set",
			q{
				alias Unit = void[0];
				enum unit = Unit.init;
				Unit[ulong] set;
			},
			q{ set[x] = unit; },
			q{ auto b = x in set; },
			q{ set.remove(x); },
		)(n, mode);
	}
}

err_t benchmarkRedBlackTree(int n, char mode) {
	version (D_BetterC) {
		assert(0, "no std.container.rbtree in betterC mode");
	} else {
		return benchmarkUpsertReadRemove!("std.container.rbtree",
			q{
				import std.container.rbtree;
				auto set = redBlackTree!ulong();
			},
			q{ set.insert(x); },
			q{ auto b = x in set; },
			q{ set.removeKey(x); },
		)(n, mode);
	}
}

err_t benchmarkBTree(int n, char mode) {
	version (D_BetterC) {
		assert(0, "FIXME: no eris.set:BTree in betterC (yet)");
	} else {
		return benchmarkUpsertReadRemove!("eris:BTree",
			q{
				import eris.btree;
				BTree!ulong set;
			},
			q{ set.upsert(x); },
			q{ auto p = x in set; },
			q{ assert(0, "FIXME: implement BTree removal"); },
		)(n, mode);
	}
}
