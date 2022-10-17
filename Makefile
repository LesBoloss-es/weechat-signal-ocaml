all:
	dune build
	make -C src/plugin clean
	make -C src/plugin plugin.so

clean:
	dune clean
	make -C src/plugin clean
