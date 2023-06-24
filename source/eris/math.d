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
	// ^ mean and M2 accumulators used in Welford's online variance calculation algorithm
	// ref: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm

	import core.math : sqrt;
	import std.math.traits : isNaN;

 pragma(inline) nothrow @nogc pure public:
	/// Accumulate a given (non-NaN) value.
	void opOpAssign(string op : "+")(double x)
	in (!isNaN(x))
	{
		_count += 1;
		_min = x < _min ? x : _min;
		_max = x > _max ? x : _max;
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
	double min() const in (count > 0) => _min;

	/// Biggest accumulated value.
	double max() const in (count > 0) => _max;

	/// Average accumulated value.
	double mean() const in (count > 0) => _mean;

	/// Population variance.
	double variance() const in (count > 0) => _m2 / _count;

	/// Sample variance.
	double var() const in (count > 1) => _m2 / (_count - 1);

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
