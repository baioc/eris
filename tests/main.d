module main;

import core.stdc.string : strcmp;
import core.stdc.stdlib : atoi, rand, srand, malloc, free;
import core.stdc.time : clock, clock_t, CLOCKS_PER_SEC;
import core.stdc.stdio : printf;

alias cstring = const(char)*;


version (D_Coverage) {} else extern(C) int main(int argc, cstring* argv) {
	cstring[] args = argv[0 .. argc];
	if (argc <= 1 || strcmp(args[1], "unittest") == 0) return unittests();
	else if (strcmp(args[1], "benchmark") == 0)        return benchmark(args[2..$]);
	return 1;
}

int unittests() {
	enum string[] moduleNames = [
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


int benchmark(const(cstring[]) args) {
	const n = args.length;
	if (n < 1) return 1;
	const cstring type = args[0];
	if (strcmp(type, "aaset") == 0 && n >= 3)  return benchmarkAASet(atoi(args[1]), args[2][0]);
	if (strcmp(type, "rbtree") == 0 && n >= 3) return benchmarkRBTree(atoi(args[1]), args[2][0]);
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


int benchmarkAASet(int n, char mode) {
	if (n < 0) return 1;
	version (D_BetterC) {} else {
		double elapsedMs = (const(int[]) inputs, out begin, out end){
			import eris.set : AASet;
			auto set = new AASet!ulong();
			switch (mode) {
				case 'w': // only measure insertions
					begin = clock();
					foreach (i; 0 .. n) set.upsert(inputs[i]);
					end = clock();
					break;

				case 'r': // only measure reads
					foreach (i; 0 .. n) set.upsert(inputs[i]);
					begin = clock();
					foreach (i; 0 .. n) auto b = set.contains(inputs[i]);
					end = clock();
					break;

				case 'x': // measure insert+read+remove
					begin = clock();
					foreach (i; 0 .. n) set.upsert(inputs[i]);
					foreach (i; 0 .. n) auto b = set.contains(inputs[i]);
					foreach (i; 0 .. n) set.remove(inputs[i]);
					end = clock();
					break;

				default: assert(0);
			}
		}.microBenchMark(n);
		printf("AASet: %.3f ns/element\n", elapsedMs * 1e6 / n);
	}
	return 0;
}

int benchmarkRBTree(int n, char mode) {
	if (n < 0) return 1;
	version (D_BetterC) {} else {
		double elapsedMs = (const(int[]) inputs, out begin, out end){
			import std.container.rbtree;
			auto set = redBlackTree!ulong();
			switch (mode) {
				case 'w': // only measure insertions
					begin = clock();
					foreach (i; 0 .. n) set.insert(inputs[i]);
					end = clock();
					break;

				case 'r': // only measure reads
					foreach (i; 0 .. n) set.insert(inputs[i]);
					begin = clock();
					foreach (i; 0 .. n) auto b = inputs[i] in set;
					end = clock();
					break;

				case 'x': // measure insert+read+remove
					begin = clock();
					foreach (i; 0 .. n) set.insert(inputs[i]);
					foreach (i; 0 .. n) auto b = inputs[i] in set;
					foreach (i; 0 .. n) set.removeKey(inputs[i]);
					end = clock();
					break;

				default: assert(0);
			}
		}.microBenchMark(n);
		printf("RedBlackTree: %.3f ns/element\n", elapsedMs * 1e6 / n);
	}
	return 0;
}
