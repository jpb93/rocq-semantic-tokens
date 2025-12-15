FILE ?= test.v

.PHONY: build test clean run

build:
	dune build

test:
	dune runtest

clean:
	dune clean
	rm -f lexer
	rm -f bin/*.cmi bin/*.cmx bin/*.o
	rm -f lib/*.cmi lib/*.cmx lib/*.o

run:
	dune exec coq_tokens -- $(FILE)

# Install to your system (optional)
install:
	dune install
