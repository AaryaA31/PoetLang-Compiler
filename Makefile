all : poetlang.out

##############################
#
# PoetLang Interpreter
#
# Compilation:
# Option 1: Simply type "make" to compile the interpreter
# Option 2: Use "ocamlbuild poetlang.native" to build manually
#
# Test input is in poetlang.tb
# Output is saved to poetlang.out
#

poetlang : scanner.cmo parser.cmo poetlang.cmo ast.cmo
	ocamlc -w A -o poetlang $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli ast.cmi
parser.cmo: parser.ml

scanner.cmi : parser.cmi

poetlang.out : poetlang poetlang.tb
	./poetlang < poetlang.tb > poetlang.out

# Compile interface files explicitly
scanner.cmo : scanner.cmi
poetlang.cmo : poetlang.cmi

# OCaml dependency hints (optional)
poetlang.cmo : scanner.cmo parser.cmi ast.cmi
parser.cmo : ast.cmi parser.cmi
scanner.cmo : parser.cmi

##############################
.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml poetlang.out poetlang
