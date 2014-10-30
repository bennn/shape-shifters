definitions:
	ocamlc definitions.ml

test:
	ocamlc definitions.ml classes.ml wellformed_test.ml
	./a.out
	ocamlc definitions.ml classes.ml subtype_test.ml
	./a.out

clean:
	rm *.cm*
	rm a.out
