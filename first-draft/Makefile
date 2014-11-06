definitions:
	ocamlc definitions.ml

test:
	ocamlc definitions.ml classes.ml booleans.ml life_forms.ml simple_shapes.ml wellformed_test.ml
	./a.out
	ocamlc definitions.ml classes.ml booleans.ml subtype_test.ml
	./a.out

clean:
	rm *.cm*
	rm a.out
