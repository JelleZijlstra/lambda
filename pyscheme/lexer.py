T_EOF = 0
T_STRING = 1
T_NAME = 2
T_INT = 4
T_FLOAT = 5
T_RATIONAL = 6
T_COMPLEX = 7
T_BOOL = 8

from peeking_iterator import peeking_iterator

class lexer_error(Exception):
	pass

def is_whitespace(c):
	return c in (' ', '\t', '\n', '\r')

def is_digit(c):
	return c in ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

def consume_while(it, pred):
	"""Eat all characters out of the iterator while predicate pred holds true; returns their concatenation"""
	out = ""
	while True:
		c = it.peek()
		if pred(c):
			out += c
			it.next()
		else:
			return out

def tokenize(string):
	it = peeking_iterator(iter(string), end_default=T_EOF)
	while it.has_next():
		c = it.next()
		# EOF
		if c == T_EOF:
			yield T_EOF, None
		# ignore whitespace
		elif is_whitespace(c):
			continue
		# single-character tokens
		elif c == '(' or c == ')' or c == '\'' or c == '.':
			yield c, None
		# strings
		elif c == '"':
			string = ""
			# set to true when an \ is encountered
			escaped = False
			while True:
				if not it.has_next():
					raise lexer_error("Unescaped EOF within string: %s" % string)
				c = it.next()
				if escaped:
					if c == 'n':
						string += '\n'
					elif c == 't':
						string += '\t'
					else:
						string += c
					escaped = False
				else:
					if c == '\\':
						escaped = True
					elif c == '"':
						break
					else:
						string += c

			yield T_STRING, string
		# numeric literals
		elif is_digit(c):
			string = c + consume_while(it, is_digit)
			c = it.peek()
			# float
			if c == '.':
				it.next()
				snd_part = consume_while(it, is_digit)
				literal = string + '.' + snd_part
				yield T_FLOAT, float(literal)
			# rational
			elif c == '/':
				it.next()
				snd_part = consume_while(it, is_digit)
				if snd_part == "":
					raise(lexer_error("Invalid rational literal: %s/" % string))
				yield T_RATIONAL, (int(string), int(snd_part))
			# complex literal
			elif c == '+':
				it.next()
				snd_part = consume_while(it, is_digit)
				if snd_part == "":
					raise(lexer_error("Invalid complex literal (missing second part): %s+" % string))
				following = it.next()
				if following != 'i':
					raise(lexer_error("Invalid complex literal (no following 'i'): %s+%s" % (string, snd_part)))
				yield T_COMPLEX, (int(string), int(snd_part))
			# integer
			else:
				yield T_INT, int(string)
		# booleans
		elif c == '#':
			c = it.next()
			if c == 't':
				yield T_BOOL, True
			elif c == 'f':
				yield T_BOOL, False
			else:
				raise(lexer_error("Invalid # literal"))
		# comments
		elif c == ';':
			consume_while(it, lambda c: c != '\n')
		# names
		else:
			name = c + consume_while(it, lambda c: not is_whitespace(c) and c not in ('(', ')', T_EOF))
			yield T_NAME, name

def print_tokens(string):
	for tkn in tokenize(string):
		print(tkn)

