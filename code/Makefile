
all: clean build run

parser: clean
	ocamlyacc parser.mly

build: 
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 photon.native

run:
	./photon.native < input.txt
	./photon.native < helloworld.txt

clean :
	rm -rf _build photon.native parser.ml parser.mli
