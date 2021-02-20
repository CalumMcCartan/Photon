
all: build run

build: 
	ocamlbuild photon.native

run:
	./photon.native < input.txt

clean :
	rm -rf _build photon.native
