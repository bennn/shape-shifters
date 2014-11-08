.PHONY: test

all:
	ocamlc definitions.ml subtype.ml well_formed.ml

test:

	ocamlc -I test definitions.ml subtype.ml well_formed.ml test/test_utils.ml test/object.ml test/boolean.ml test/mf_boolean.ml test/number.ml test/sample1.ml test/test.ml
	./a.out
