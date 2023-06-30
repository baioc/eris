DC ?= ldmd2
CXX ?= g++

DFLAGS ?= \
	-preview=fieldwise \
	-O -release -inline -boundscheck=off -g \
	-betterC -static --fvisibility=hidden \
	-i

CXXFLAGS ?= \
	-std=c++20 \
	-O2 -g \
	-static -fvisibility=hidden -fvisibility-inlines-hidden


.PHONY: clean build

# default
build: sets.exe

liberis.a: liberis.d ../../source/eris/math.d
	$(DC) $(DFLAGS) -I '../../source/' -lib -of "$@" $<

sets.exe: sets.cpp eris.h liberis.a
	$(CXX) $(CXXFLAGS) -o $@ $< liberis.a

clean:
	- rm liberis.a
	- rm sets.exe
