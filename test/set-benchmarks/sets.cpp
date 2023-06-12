#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdio.h>
#include <stdint.h>

#include <set>
#include <unordered_set>
#include <functional>
#include <memory>

extern "C" {
#include "eris.h"
}


size_t fnv_1a(const void *ptr, size_t n) {
	uint32_t hash = 2166136261u;
	const unsigned char *bytes = static_cast<const unsigned char *>(ptr);
	for (size_t i = 0; i < n; ++i) {
		hash ^= *bytes++;
		hash *= 16777619;
	}
	return hash;
}


Accumulator benchmark(
	std::function<void (clock_t&, clock_t&)> code,
	unsigned replications = 30
) {
	auto acc = ACCUMULATOR_INIT;
	for (long rep = 0; rep < replications; ++rep) {
		clock_t begin, end;
		code(begin, end);
		double elapsedUs = (end - begin) * 1e6 / CLOCKS_PER_SEC;
		accumulator_put(&acc, elapsedUs);
	}
	return acc;
}



struct String32 {
	char chars[32];
	bool operator==(const String32 &other) const {
		return strncmp(this->chars, other.chars, sizeof(this->chars)) == 0;
	}
	struct Less {
		bool operator()(const String32& a, const String32& b) const {
			return strncmp(a.chars, b.chars, sizeof(a.chars)) < 0;
		}
	};
	struct Hash {
		bool operator()(const String32& self) const {
			return fnv_1a(self.chars, sizeof(self.chars));
		}
	};
};

void randomize(int *output) {
	*output = rand();
}

void randomize(String32 *output) {
	snprintf(output->chars, sizeof(output->chars), "%031d", rand());
}

template <template <typename, typename> class Set, typename T, typename Aux>
int setBenchmarks(const char *container, const char *element, int n, unsigned seed = 0) {
	if (n < 0) return __LINE__;
	const auto n2 = n * 2;

	auto buffer = std::make_unique<T[]>(n2);
	if (buffer == nullptr) return __LINE__;

	srand(seed);
	auto benchmarkRandomized = [&](std::function<void (clock_t&, clock_t&)> code){
		return benchmark([&](clock_t& begin, clock_t& end){
			for (int i = 0; i < n2; ++i) randomize(&buffer[i]);
			code(begin, end);
		});
	};

	volatile unsigned long store;

	auto upsert = benchmarkRandomized([&](clock_t& begin, clock_t& end){
		Set<T,Aux> set;
		begin = clock();
		for (int i = 0; i < n; ++i) {
			auto x = buffer[i];
			set.upsert(x);
		}
		end = clock();
	});

	auto lookupFind = benchmarkRandomized([&](clock_t& begin, clock_t& end){
		Set<T,Aux> set;
		for (int i = 0; i < n; ++i) {
			auto x = buffer[i];
			set.upsert(x);
		}
		begin = clock();
		for (int i = 0; i < n; ++i) {
			auto x = buffer[i];
			auto b = set.contains(x);
			store = b;
		}
		end = clock();
	});

	auto lookupFail = benchmarkRandomized([&](clock_t& begin, clock_t& end){
		Set<T,Aux> set;
		for (int i = 0; i < n2; ++i) {
			auto x = buffer[i];
			set.upsert(x);
		}
		for (int i = 0; i < n; ++i) {
			auto x = buffer[i];
			set.remove(x);
		}
		begin = clock();
		for (int i = 0; i < n; ++i) {
			auto x = buffer[i];
			auto b = set.contains(x);
			store = b;
		}
		end = clock();
	});

	auto remove = benchmarkRandomized([&](clock_t& begin, clock_t& end){
		Set<T,Aux> set;
		for (int i = 0; i < n; ++i) {
			auto x = buffer[i];
			set.upsert(x);
		}
		begin = clock();
		for (int i = 0; i < n; ++i) {
			auto x = buffer[i];
			set.remove(x);
		}
		end = clock();
	});

#define PRINT_RESULT(OP) \
	printf( \
		"container=%s\telement=%s\toperation=%s\tn=%d\tavg=%.3f\tstd=%.3f\tmin=%.3f\tmax=%.3f\n", \
		container, element, #OP, n, accumulator_mean(&(OP)), \
		accumulator_std(&(OP)), accumulator_min(&(OP)), accumulator_max(&(OP)) \
	)
	PRINT_RESULT(upsert);
	PRINT_RESULT(lookupFind);
	PRINT_RESULT(lookupFail);
	PRINT_RESULT(remove);
#undef PRINT_RESULT

	return 0;

}


template <typename T, typename Comp>
struct StdSet {
	std::set<T,Comp> treeSet;
	void upsert(T x) { treeSet.insert(std::move(x)); }
	bool contains(const T& x) const { return treeSet.count(x) != 0; }
	void remove(const T& x) { treeSet.erase(x); }
};

int benchmarkStdSet(int n) {
	return setBenchmarks<StdSet, int, std::less<int>>("std::set", "int", n)
	     + setBenchmarks<StdSet, String32, String32::Less>("std::set", "String32", n);
}

template <typename T, typename Hash>
struct StdUnorderedSet {
	std::unordered_set<T,Hash> hashSet;
	void upsert(T x) { hashSet.insert(std::move(x)); }
	bool contains(const T& x) const { return hashSet.count(x) != 0; }
	void remove(const T& x) { hashSet.erase(x); }
};

int benchmarkStdUnorderedSet(int n) {
	return setBenchmarks<StdUnorderedSet, int, std::hash<int>>("std::unordered_set", "int", n)
	     + setBenchmarks<StdUnorderedSet, String32, String32::Hash>("std::unordered_set", "String32", n);
}


int main(int argc, char const *argv[]) {
	if (argc <= 2) return __LINE__;
	const char *type = argv[1];
	int size = atoi(argv[2]);
	if (strcmp(type, "std_set") == 0)           return benchmarkStdSet(size);
	if (strcmp(type, "std_unordered_set") == 0) return benchmarkStdUnorderedSet(size);
	return __LINE__;
}
