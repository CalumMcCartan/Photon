
all: clean build run

parser: clean
	ocamlyacc parser.mly

build: 
	ocamlbuild photon.native

run:
	./photon.native < input.txt

clean :
	rm -rf _build photon.native parser.ml parser.mli
