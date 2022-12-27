test: main.hs RandomGraph.hs BloomFilter.hs ROBDD.hs
	ghc -o test -odir objs -hidir objs --make -O3 main.hs

clean:
	rm -rf objs
	rm -f test

