# Largely stolen from IMP
MAIN=lambda
CAMLCFLAGS= -g

OBJS = ast.cmo lexer.cmo parser.cmo eval.cmo main.cmo

%.cmo : %.ml
	ocamlc $(CAMLCFLAGS) -c $<

%.cmi : %.mli
	ocamlc $(CAMLCFLAGS) -c $<


$(MAIN): $(OBJS)
	ocamlc $(CAMLCFLAGS) -o $(MAIN) $(OBJS)

lexer.ml : lexer.mll
	ocamllex -q $<

lexer.cmo : parser.cmi lexer.ml
	ocamlc $(CAMLCFLAGS) -c lexer.ml

parser.ml : parser.mly
	ocamlyacc -q $<

parser.mli : parser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN)
