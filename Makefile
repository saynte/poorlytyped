all: rayhs raycpp

rayhs: ray.hs
	ghc ${GHC_OPT} --make $< -o $@

raycpp: ray.cpp
	g++ -O3 -ffast-math -fopenmp $< -o $@

tests: cpptests.txt hstests.txt

cpptests.txt: raycpp
	sh test.sh &> cpptests.txt

hstests.txt: rayhs
	sh tesths.sh &> hstests.txt
