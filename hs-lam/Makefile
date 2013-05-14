# Largely stolen from IMP
MAIN=hslam
HSC=ghc
HSCFLAGS=--make

all: $(MAIN)

$(MAIN): lexer.hs parser.hs
	$(HSC) $(HSCFLAGS) main.hs -o $(MAIN)

lexer.hs: lexer.x
	alex $<

parser.hs: parser.y
	happy $<

clean:
	rm -f *.hi parser.hs lexer.hs main hslam
