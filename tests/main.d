module main;

alias cstring = const(char)*;


version (D_BetterC) extern(C) int main(int argc, cstring* argv) {
	cstring[] args = argv[0 .. argc];
	import core.stdc.string : strcmp;
	if (argc <= 1 || strcmp(args[1], "unittest") == 0) return unittests();
	else if (strcmp(args[1], "benchmark") == 0)        return benchmark(args[2..$]);
	else                                               return 1;
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


int benchmark(cstring[] args) {
	import core.stdc.stdio : puts;
	foreach (s; args) puts(s);
	return 0;
}
