import parser
from lexer import *

pi = peeking_iterator(iter("hello"))
for x in pi:
	print(x)
	print(pi.has_next())
	if not pi.has_next():
		break

print([x for x in tokenize("hello")])

parser.parse("hello", do_print=True)
parser.parse("(+ 1 2)", do_print=True)
