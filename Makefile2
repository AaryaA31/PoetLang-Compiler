all: test.out

test: ast.cmo parser.cmo scanner.cmo test.cmo
	ocamlc -w A -o test ast.cmo parser.cmo scanner.cmo test.cmo


test.out: test example.mc
	./test < example.mc > test.out


%.cmi: %.mli
	ocamlc -w A -c $<


%.cmo %.cmi: %.ml
	ocamlc -w A -c $<


scanner.ml: scanner.mll
	ocamllex $<

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -w A -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -w A -c parser.ml


scanner.cmo: scanner.ml parser.cmi
scanner.cmi: scanner.ml

test.cmo test.cmi: test.ml scanner.cmo parser.cmi ast.cmi

ast.cmo ast.cmi: ast.ml

##############################
.PHONY: clean
clean:
	rm -rf *.cmi *.cmo test scanner.ml parser.ml parser.mli
