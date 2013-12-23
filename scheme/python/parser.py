#!/opt/local/bin/python3.3

from lexer import *
from ast import *
from peeking_iterator import peeking_iterator

grammar = """

file_input: stmt* ENDMARKER

stmt: literal | list | quoted_list | NAME
literal: '#t' | '#f' | STRING | NUMBER
list: '(' stmt* ')'
quoted_list: '\'' list

"""

class parse_error(Exception):
	pass

def do_parse_list(it):
	lst = []
	while True:
		c, cnt = it.peek()
		if c == ')':
			it.next()
			break
		elif c == '.':
			it.next()
			rest = do_parse(it)
			if isinstance(rest, slist):
				lst = lst + rest.lst
			elif isinstance(rest, name):
				lst.append(dotted_name(rest))
			else:
				raise parse_error("Unexpected expression following dot")
			n, cnt = it.next()
			if n != ')':
				raise parse_error("Expected ) following dot")
			break
		elif c == T_EOF:
			raise parse_error("Unexpected EOF within list context")
		else:
			lst.append(do_parse(it))
	return slist(lst)

def do_parse(it):
	tkn, content = it.next()
	if tkn == "'":
		return quoted(do_parse(it))
	elif tkn == '(':
		return do_parse_list(it)
	elif tkn == ')':
		raise parse_error("Unexpected )")
	elif tkn in (T_STRING, T_INT, T_FLOAT, T_COMPLEX, T_BOOL, T_RATIONAL):
		return literal(tkn, content)
	elif tkn == T_EOF:
		raise parse_error("Unexpected EOF")
	elif tkn == T_NAME:
		return name(content)
	else:
		raise parse_error("Unexpected token: " + str(tkn))

def do_parse_program(it):
	prgrm = []
	while True:
		c, cnt = it.peek()
		if c == T_EOF:
			return program(prgrm)
		else:
			prgrm.append(do_parse(it))

def parse(str, do_print=False):
	tokenizer = tokenize(str)
	pi = peeking_iterator(tokenizer, end_default=T_EOF)
	program = do_parse_program(pi)
	if do_print:
		program.pretty_print()
	return program

