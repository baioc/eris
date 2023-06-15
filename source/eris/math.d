/// Miscellaneous numeric math.
module eris.math;


/// Tracks a stream of values and computes some statistics in constant space.
struct Accumulator {
 private:
	size_t _count = 0;
	double _min = double.infinity;
	double _max = -double.infinity;
	double _mean = 0;
	double _m2 = 0;
	// mean and M2 accumulators used in Welford's online variance calculation algorithm
	// ref: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm

 pragma(inline) nothrow @nogc pure public:
	/// Accumulate a given value. Ignores `NaN`s.
	void opOpAssign(string op : "+")(in double x) {
		import std.math.operations : fmin, fmax;
		import std.math.traits : isNaN;
		if (isNaN(x)) return;
		_count += 1;
		_min = fmin(_min, x);
		_max = fmax(_max, x);
		const delta = x - _mean;
		_mean += delta / _count;
		const delta2 = x - _mean;
		_m2 += delta * delta2;
	}

	/// ditto
	alias put = opOpAssign!"+";

 @property:
	/// Number of accumulated values.
	size_t count() const => _count;

	/// Smallest accumulated value.
	double min() const => _min;

	/// Biggest accumulated value.
	double max() const => _max;

	/// Average accumulated value.
	double mean() const => _mean;

	/// Population variance.
	double variance() const => count > 0 ? _m2 / _count : double.init;

	/// Sample variance.
	double var() const => count > 1 ? _m2 / (_count - 1) : double.init;

	private import core.math : sqrt;

	/// Population standard deviation.
	double standardDeviation() const => sqrt(this.variance);

	/// Sample standard deviation.
	double std() const => sqrt(this.var);
}

///
nothrow @nogc unittest {
	import std.range.primitives : put, isOutputRange;
	import std.math.operations : isClose;

	Accumulator small, big;
	static assert(isOutputRange!(Accumulator, double));

	static const double[] smallNumbers = [ 4, 7, 13, 16 ];
	put(small, smallNumbers);

	static const double[] bigNumbers = [ // same as smallnumbers + 1e9
		1.000000004e9, 1.000000007e9, 1.000000013e9, 1.000000016e9 ];
	put(big, bigNumbers);

	assert(isClose(small.count,             big.count            ));
	assert(isClose(small.min  + 1e9,        big.min              ));
	assert(isClose(small.max  + 1e9,        big.max              ));
	assert(isClose(small.mean + 1e9,        big.mean             ));
	assert(isClose(small.var,               big.var              ));
	assert(isClose(small.variance,          big.variance         ));
	assert(isClose(small.std,               big.std              ));
	assert(isClose(small.standardDeviation, big.standardDeviation));
}
