GHC_OPTS=--make -O2 -fglasgow-exts -optc-mfpmath=sse -optc-msse2 -optc-O3 -threaded


all: mandeltest

mandeltest: MandelTest.hs Mandel.hs MandelOrig.hs
	ghc ${GHC_OPTS} MandelTest.hs -o mandeltest

test: mandeltest
	./mandeltest -k win

test-threaded: mandeltest
	./mandeltest -k png:800x600 +RTS -N4 -RTS
