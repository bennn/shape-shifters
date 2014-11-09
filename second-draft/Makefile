.PHONY: test

all:
	ocamlc definitions.ml subtype.ml well_formed.ml

test:
	ocamlbuild test/test.d.byte
	./test.d.byte

clean:
	ocamlbuild -clean 
