# Largely stolen from IMP
MAIN=ocamlscheme
CAMLC=ocamlc
CAMLCFLAGS= -g
CAMLOPT=ocamlopt
CAMLOPTFLAGS=

SOURCES = environment.ml ast.ml parser.ml lexer.ml util.ml eval.ml library.ml main.ml
OBJECTS = $(SOURCES:.ml=.cmo)
INTERFACES = $(SOURCES:.ml=.mli)
INTERFACE_OBJECTS = $(SOURCES:.ml=.cmi)

LIBS=str.cma

all: $(MAIN)

%.cmi: %.mli
	$(CAMLC) $(CAMLCFLAGS) -c $<

%.cmo: %.ml
	$(CAMLC) $(CAMLCFLAGS) -c $<

%.cmx: %.ml
	$(CAMLOPT) $(CAMLOPTFLAGS) -c $<

$(MAIN): $(INTERFACE_OBJECTS) $(OBJECTS)
	$(CAMLC) $(CAMLCFLAGS) -o $(MAIN) $(LIBS) $(OBJECTS)

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
	rm -f *.cmo *.cmi *.cmx *.o lexer.ml parser.ml parser.mli $(MAIN)
