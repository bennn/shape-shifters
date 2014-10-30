definitions:
	ocamlc definitions.ml

test:
	ocamlc definitions.ml classes.ml subtype_test.ml
	./a.out
