# Largely stolen from IMP
MAIN=lambda
MAINOPT=lambdaopt
CAMLC=ocamlc
CAMLCFLAGS= -g
CAMLOPT=ocamlopt
CAMLOPTFLAGS=

SOURCES = ast.ml parser.ml lexer.ml util.ml builtin.ml typecheck.ml eval.ml compile.ml main.ml
OBJECTS = $(SOURCES:.ml=.cmo)
INTERFACES = $(SOURCES:.ml=.mli)
INTERFACE_OBJECTS = $(SOURCES:.ml=.cmi)
OPT_OBJECTS = $(SOURCES:.ml=.cmx)

LIBS=str.cma
OPT_LIBS=str.cmxa

all: $(MAIN) $(MAINOPT)

%.cmi: %.mli
	$(CAMLC) $(CAMLCFLAGS) -c $<

%.cmo: %.ml
	$(CAMLC) $(CAMLCFLAGS) -c $<

%.cmx: %.ml
	$(CAMLOPT) $(CAMLOPTFLAGS) -c $<

$(MAIN): $(INTERFACE_OBJECTS) $(OBJECTS)
	$(CAMLC) $(CAMLCFLAGS) -o $(MAIN) $(LIBS) $(OBJECTS)

$(MAINOPT): $(INTERFACE_OBJECTS) $(OPT_OBJECTS)
	$(CAMLOPT) $(CAMLOPTFLAGS) -o $(MAINOPT) $(OPT_LIBS) $(OPT_OBJECTS)

lexer.ml: lexer.mll
	ocamllex -q $<

lexer.mli: lexer.mll
	ocamllex -q $<

lexer.cmo: parser.cmi lexer.ml
	ocamlc $(CAMLCFLAGS) -c lexer.ml

parser.ml: parser.mly
	ocamlyacc -q $<

parser.mli: parser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi *.cmx *.o lexer.ml parser.ml parser.mli $(MAIN) $(MAINOPT)
