# Largely stolen from IMP
MAIN=lambda
LAMBDAC=lambdac
CAMLCFLAGS= -g

OBJS = ast.cmo lexer.cmo parser.cmo util.cmo
MAIN_OBJS = $(OBJS) eval.cmo main.cmo
LAMBDAC_OBJS = $(OBJS) compile.cmo lambdac.cmo
LIBS = str.cma

%.cmo : %.ml
	ocamlc $(CAMLCFLAGS) -c $<

%.cmi : %.mli
	ocamlc $(CAMLCFLAGS) -c $<


$(MAIN): $(MAIN_OBJS)
	ocamlc $(CAMLCFLAGS) -o $(MAIN) $(LIBS) $(MAIN_OBJS)

$(LAMBDAC): $(LAMBDAC_OBJS)
	ocamlc $(CAMLCFLAGS) -o $(LAMBDAC) $(LIBS) $(LAMBDAC_OBJS)

lexer.ml : lexer.mll
	ocamllex -q $<

lexer.cmo : parser.cmi lexer.ml
	ocamlc $(CAMLCFLAGS) -c lexer.ml

parser.ml : parser.mly
	ocamlyacc -q $<

parser.mli : parser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN) $(LAMBDAC)
