module liberis;

import eris.math : Accumulator;


export extern(C) {

	__gshared immutable ACCUMULATOR_INIT = Accumulator.init;
	void accumulator_put(Accumulator* self, double x) => self.put(x);
	size_t accumulator_count(const Accumulator* self) => self.count;
	double accumulator_min(const Accumulator* self) => self.min;
	double accumulator_max(const Accumulator* self) => self.max;
	double accumulator_mean(const Accumulator* self) => self.mean;
	double accumulator_variance(const Accumulator* self) => self.variance;
	double accumulator_var(const Accumulator* self) => self.var;
	double accumulator_standard_deviation(const Accumulator* self) => self.standardDeviation;
	double accumulator_std(const Accumulator* self) => self.std;

}
