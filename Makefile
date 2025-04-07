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

# Interface rules (only if .mli exists)
%.cmi: %.mli
	ocamlc -w A -c $<

# Implementation rules (generates .cmi and .cmo)
%.cmo %.cmi: %.ml
	ocamlc -w A -c $<

# Scanner and parser generation
scanner.ml: scanner.mll
	ocamllex $<

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

# Explicit dependency tracking
parser.cmi: parser.mli ast.cmi
parser.cmo: parser.ml

scanner.cmo: scanner.ml parser.cmi
scanner.cmi: scanner.ml

poetlang.cmo: poetlang.ml scanner.cmo parser.cmi ast.cmi
poetlang.cmi: poetlang.ml

ast.cmo ast.cmi: ast.ml

##############################
.PHONY: clean
clean:
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml poetlang poetlang.out
