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


size_t fnv(const void *ptr, size_t n) {
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
			return fnv(self.chars, sizeof(self.chars));
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
	for (int i = 0; i < n2; ++i) randomize(&buffer[i]);

	auto upsert = benchmark([&](clock_t& begin, clock_t& end){
		auto set = Set<T,Aux>();
		begin = clock();
		for (int i = 0; i < n; ++i) {
			auto& x = buffer[i];
			set.upsert(x);
		}
		end = clock();
	});

	auto lookupFind = benchmark([&](clock_t& begin, clock_t& end){
		auto set = Set<T,Aux>();
		for (int i = 0; i < n; ++i) {
			auto& x = buffer[i];
			set.upsert(x);
		}
		begin = clock();
		for (int i = 0; i < n; ++i) {
			auto& x = buffer[i];
			if (!set.contains(x)) exit(__LINE__);
		}
		end = clock();
	});

	int rss = 0;
	auto lookupFail = benchmark([&](clock_t& begin, clock_t& end){
		auto set = Set<T,Aux>();
		for (int i = 0; i < n2; ++i) {
			auto& x = buffer[i];
			set.upsert(x);
		}
		rss = set.length() > rss ? set.length() : rss;
		for (int i = 0; i < n; ++i) {
			auto& x = buffer[i];
			set.remove(x);
		}
		begin = clock();
		for (int i = 0; i < n; ++i) {
			auto& x = buffer[i];
			if (set.contains(x)) exit(__LINE__);
		}
		end = clock();
	});

	auto remove = benchmark([&](clock_t& begin, clock_t& end){
		auto set = Set<T,Aux>();
		for (int i = 0; i < n; ++i) {
			auto& x = buffer[i];
			set.upsert(x);
		}
		begin = clock();
		for (int i = 0; i < n; ++i) {
			auto& x = buffer[i];
			set.remove(x);
		}
		end = clock();
	});

#define PRINT_RESULT(OP) \
	printf( \
		"container=%s\telement=%s\toperation=%s\tn=%d\tres=%d\tavg=%.3f\tstd=%.3f\tmin=%.3f\tmax=%.3f\n", \
		container, element, #OP, n, rss, accumulator_mean(&(OP)), \
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
	std::size_t length() const { return treeSet.size(); }
};

int benchmarkStdSet(const char *element, int n) {
	if (strcmp(element, "int") == 0) {
		return setBenchmarks<StdSet, int, std::less<int>>("std::set", "int", n);
	} else if (strcmp(element, "String32") == 0) {
		return setBenchmarks<StdSet, String32, String32::Less>("std::set", "String32", n);
	}
	return __LINE__;
}

template <typename T, typename Hash>
struct StdUnorderedSet {
	std::unordered_set<T,Hash> hashSet;
	void upsert(T x) { hashSet.insert(std::move(x)); }
	bool contains(const T& x) const { return hashSet.count(x) != 0; }
	void remove(const T& x) { hashSet.erase(x); }
	std::size_t length() const { return hashSet.size(); }
};

int benchmarkStdUnorderedSet(const char *element, int n) {
	if (strcmp(element, "int") == 0) {
		return setBenchmarks<StdUnorderedSet, int, std::hash<int>>("std::unordered_set", "int", n);
	} else if (strcmp(element, "String32") == 0) {
		return setBenchmarks<StdUnorderedSet, String32, String32::Hash>("std::unordered_set", "String32", n);
	}
	return __LINE__;
}


int main(int argc, char const *argv[]) {
	if (argc <= 3) return __LINE__;
	const char *container = argv[1];
	const char *element = argv[2];
	int n = atoi(argv[3]);
	if (strcmp(container, "std_set") == 0)           return benchmarkStdSet(element, n);
	if (strcmp(container, "std_unordered_set") == 0) return benchmarkStdUnorderedSet(element, n);
	return __LINE__;
}
