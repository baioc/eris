/// Metaprogramming helpers for type and code generation.
module eris.typecons;


/// Generate fluent setters for each (given) field of an aggregate type.
mixin template makeFluent(Aggregate, fieldNames...) {
	import std.ascii : toUpper;
	static foreach (fieldName; fieldNames) {
		mixin(
			`pragma(inline) ` ~
			Aggregate.stringof ~ ` with` ~ toUpper(fieldName[0])~fieldName[1..$] ~ `(` ~
				Aggregate.stringof ~ ` self,` ~
				`typeof(` ~ Aggregate.stringof ~ `.` ~ fieldName ~ `) value
			) {
				self.` ~ fieldName ~ ` = value;
				return self;
			}`
		);
	}
}

/// ditto
mixin template makeFluent(T) if (__traits(isPOD, T)) {
	import std.traits : FieldNameTuple;
	mixin makeFluent!(T, FieldNameTuple!T);
}

///
unittest {
	struct S { int someField = 0; }
	mixin makeFluent!S; // NB: UFCS only applies to module-scoped functions ...
	S s = withSomeField(S.init, 1); // ... which is not the case in this unittest
	assert(s.someField == 1);
}
