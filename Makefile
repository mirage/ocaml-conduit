.PHONY: build clean test doc bench

build:
	dune build

test:
	dune runtest

clean:
	dune clean

doc:
	dune build @doc

bench:
	@dune exec  -- ./bench/cost.exe --json