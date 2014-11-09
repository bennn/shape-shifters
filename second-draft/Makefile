.PHONY: test

all:
	ocamlc definitions.ml subtype.ml well_formed.ml

test:
	ocamlbuild test/runall.d.byte
	./runall.d.byte

clean:
	ocamlbuild -clean 
