#ifndef ERIS_H
#define ERIS_H


typedef struct {
	size_t _count;
	double _min;
	double _max;
	double _mean;
	double _m2;
} Accumulator;

extern const Accumulator ACCUMULATOR_INIT;

extern void accumulator_put(Accumulator *self, double x);
extern size_t accumulator_count(const Accumulator *self);
extern double accumulator_min(const Accumulator *self);
extern double accumulator_max(const Accumulator *self);
extern double accumulator_mean(const Accumulator *self);
extern double accumulator_variance(const Accumulator *self);
extern double accumulator_var(const Accumulator *self);
extern double accumulator_standard_deviation(const Accumulator *self);
extern double accumulator_std(const Accumulator *self);


#endif // ERIS_H
