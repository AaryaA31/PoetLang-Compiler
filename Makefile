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
##############################

all: poetlang.out

poetlang.out: poetlang poetlang.tb
	./poetlang < poetlang.tb > poetlang.out

poetlang: scanner.cmo parser.cmo poetlang.cmo ast.cmo
	ocamlc -w A -o poetlang $^

# Interface rules
%.cmi: %.mli
	ocamlc -w A -c $<

# Implementation rules
%.cmo: %.ml
	ocamlc -w A -c $<

# Special rules
scanner.ml: scanner.mll
	ocamllex $<

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

# Specific dependency: ast.mli must be compiled to build parser.mli
ast.cmi: ast.mli
	ocamlc -w A -c ast.mli

# Order-sensitive compilation
parser.cmi: parser.mli ast.cmi
parser.cmo: parser.ml

scanner.cmo: scanner.ml parser.cmi
scanner.cmi: scanner.ml

poetlang.cmo: poetlang.ml poetlang.cmi scanner.cmo parser.cmi ast.cmi
poetlang.cmi: poetlang.mli

ast.cmo: ast.ml ast.cmi

##############################
.PHONY: clean
clean:
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml poetlang poetlang.out
