# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

retest:
	dos2unix testall.sh
	make clean
	clear
	make test

imagetest:
	gcc -std=c11 -Wall Image.c ImageRun.c -o ImageRun -lm
	./ImageRun

imageclean:
	rm ImageRun
	rm Shapes_gray.png
	rm sky_gray.jpg

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
	rm -rf testall.log ocamlllvm *.diff _build photon.native utils.o Image.o
	ocamlbuild -clean

utils : utils.c
	cc -o utils -DBUILD_TEST utils.c

# Building the tarball

TESTS = \
  add1 arith1 arith2 arith3 fib float1 float2 float3 for1 for2 func1 \
  func2 func3 func4 func5 func6 func7 func8 func9 gcd2 gcd global1 \
  global2 global3 hello if1 if2 if3 if4 if5 if6 local1 local2 ops1 \
  ops2 var1 var2 while1 while2

FAILS = \
  assign1 assign2 assign3 dead1 dead2 expr1 expr2 expr3 float1 float2 \
  for1 for2 for3 for4 for5 func1 func2 func3 func4 func5 func6 func7 \
  func8 func9 global1 global2 if1 if2 if3 nomain printb print \
  return1 return2 while1 while2

TESTFILES = $(TESTS:%=test-%.phn) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.phn) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags photon.ml photonparse.mly \
	README scanner.mll semant.ml testall.sh \
	utils.c arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

photon.tar.gz : $(TARFILES)
	cd .. && tar czf photon/photon.tar.gz \
		$(TARFILES:%=photon/%)
