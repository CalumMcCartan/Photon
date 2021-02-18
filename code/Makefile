
all: build run

build: 
	ocamlbuild photon.native -where

run:
	./photon.native < input.txt

clean :
	rm -rf _build
