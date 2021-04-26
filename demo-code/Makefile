# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

retest:
	dos2unix testall.sh
	make clean
	clear
	make test

# "make all" builds the executable as well as the built-in library designed
# to test linking external code

.PHONY : all
all : photon.native utils.o Image.o

# "make photon.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

photon.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind photon.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	rm -rf testall.log ocamlllvm *.diff _build photon.native utils.o Image.o images-out
	ocamlbuild -clean

utils : utils.c
	cc -o utils -DBUILD_TEST utils.c

# Building the tarball

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags photon.ml photonparse.mly \
	README scanner.mll semant.ml testall.sh \
	utils.c Image.c utils.h Image.h arcade-font.pbm font2c \
	Dockerfile

photon.tar.gz : $(TARFILES)
	cd .. && tar czf photon/photon.tar.gz \
		$(TARFILES:%=photon/%)
