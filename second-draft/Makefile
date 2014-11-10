.PHONY: test

all: test java scala paper

java:
	ocamlbuild test/java.util/run_java.d.byte
	./run_java.d.byte

paper:
	ocamlbuild  test/paper/run_paper.d.byte
	./run_paper.d.byte

scala:
	ocamlbuild test/scala.collections.immutable/run_scala.d.byte
	./run_scala.d.byte

test:
	ocamlbuild test/runall.d.byte
	./runall.d.byte

clean:
	ocamlbuild -clean 
