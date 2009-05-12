objects= expr.cmo parser.cmo lexer.cmo eval.cmo main.cmo



exec:	$(objects)
	ocamlc $(objects) -o exec



parser.ml:   parser.mly expr.cmi
	ocamlyacc $<

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml


lexer.ml:    lexer.mll parser.cmi expr.cmi
	ocamllex $<



%.cmo:	%.ml
	ocamlc -c $<
%.cmi: %ml
	ocamlc -c $<


eval.cmo:	expr.cmo


main.cmo:	expr.cmo eval.cmo parser.cmo

clean :
	rm *.cmo *.cmi lexer.ml parser.ml parser.mli

test :
	@./test.sh

