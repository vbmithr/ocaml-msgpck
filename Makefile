all:
	dune build @install @runtest

clean:
	dune clean

test:
	dune runtest --force --no-buffer

watch:
	dune build @install -w

.PHONY: test
