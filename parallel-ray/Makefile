GHC_OPT=-O2 -fforce-recomp -fexcess-precision -funbox-strict-fields -fvia-c -optc-O3 -optc-ffast-math -optc-march=native -funfolding-keeness-factor=10 -threaded -feager-blackholing


all: rayhs raycpp ray1opths ray4hs

ray4hs: ray4.hs
	ghc ${GHC_OPT} --make $< -o $@

rayhs: ray.hs
	ghc ${GHC_OPT} --make $< -o $@

ray1opths: ray1opt.hs
	ghc ${GHC_OPT} --make $< -o $@

raycpp: ray.cpp
	g++ -O3 -ffast-math -fopenmp $< -o $@

tests: cpptests.txt hstests.txt

cpptests.txt: raycpp test.sh
	sh test.sh &> cpptests.txt

hs4tests.txt: ray4hs tesths4.sh
	sh tesths4.sh &> hs4tests.txt

hstests.txt: rayhs tesths.sh
	sh tesths.sh &> hstests.txt

hs1opttests.txt: ray1opths tesths1opt.sh
	sh tesths1opt.sh &> hs1opttests.txt
