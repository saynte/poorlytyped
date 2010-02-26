GHC_OPTS=-fvia-C -O2 -optc-O2 -optc-ffast-math -fexcess-precision -optc-mfpmath=sse -optc-msse2 -funbox-strict-fields -Wall

bencher: Bencher.hs
	ghc ${GHC_OPTS} --make Bencher.hs -o bencher

fastaNew: FastaNew.hs
	ghc ${GHC_OPTS} --make FastaNew.hs -o fastanew

fastaOld: FastaOld.hs
	ghc ${GHC_OPTS} --make FastaOld.hs -o fastaold
