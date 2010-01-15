GHC_OPT=-O2 -fforce-recomp -fexcess-precision -funbox-strict-fields -fvia-c -optc-O3 -optc-ffast-math -funfolding-keeness-factor=10 -threaded -feager-blackholing


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
