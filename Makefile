cargs = -I build -g
files = build/parser.cmo build/lexer.cmo build/ast.cmo build/main.cmo

build/LogProg: $(files)
	ocamlc -o $@ $(files) $(cargs)


build/parser.cmo: parser.mly build/ast.cmo
	ocamlyacc parser.mly
	@mkdir -p build
	@mv parser.mli build/parser.mli
	@mv parser.ml build/parser.ml
	ocamlc -c build/parser.mli $(cargs)
	ocamlc -c build/parser.ml $(cargs)

build/lexer.cmo: lexer.mll
	ocamllex lexer.mll
	@mkdir -p build
	@mv lexer.ml build/lexer.ml
	ocamlc -c build/lexer.ml $(cargs)

build/ast.cmo: ast.ml
	@mkdir -p build
	ocamlc -c ast.ml $(cargs) -o build/ast

build/main.cmo: main.ml
	@mkdir -p build
	ocamlc -c main.ml $(cargs) -o build/main

.PHONY: clean
clean:
	@rm -rf build