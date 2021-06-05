all:
	dune build @install @runtest

clean:
	dune clean

watch:
	dune build @install -w
