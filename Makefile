definitions:
	ocamlc definitions.ml

test:
	ocamlc definitions.ml subtype_test.ml
	./a.out
