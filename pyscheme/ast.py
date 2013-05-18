import copy

from lexer import *

class runtime_error(Exception):
	pass

def put(s):
	print(s, end="")

class expr(object):
	pass

class quoted(expr):
	def __init__(self, e):
		super().__init__()
		self.expr = e

	def pretty_print(self):
		put("'")
		self.expr.pretty_print()

	def eval(self, context):
		return self.expr

class literal(expr):
	def __init__(self, tkn, content):
		super().__init__()
		self.tkn = tkn
		self.content = content

	def pretty_print(self):
		if self.tkn == T_STRING:
			string = '"' + self.content.replace('"', '\\"') + '"'
		elif self.tkn == T_INT or self.tkn == T_FLOAT:
			string = str(self.content)
		elif self.tkn == T_RATIONAL:
			num, denom = self.content
			string = str(num) + '/' + str(denom)
		elif self.tkn == T_COMPLEX:
			real, imag = self.content
			string = str(real) + '+' + str(imag) + 'i'
		put(string)

	def eval(self, context):
		return self

class slist(expr):
	def __init__(self, lst):
		super().__init__()
		self.lst = lst

	def pretty_print(self):
		put("(")
		for elem in self.lst:
			elem.pretty_print()
			put(" ")
		put(")")

	def eval(self, context):
		if len(self.lst) < 1:
			raise runtime_error("Empty list cannot be eval'ed")
		fst = self.lst[0]
		if isinstance(fst, name) and context.has_macro(fst.name):
			code = context.get_macro(fst.name).call(self.lst[1:], context)
			return code.eval(context)

		evaled = [elem.eval(context) for elem in self.lst]
		fn = evaled[0]
		args = evaled[1:]
		return fn.call(args)

class program(object):
	def __init__(self, prgrm):
		super().__init__()
		self.prgrm = prgrm

	def pretty_print(self):
		for line in self.prgrm:
			line.pretty_print()
			put("\n")

	def eval(self, context):
		for line in self.prgrm:
			line.eval(context)

class name(expr):
	def __init__(self, nm):
		super().__init__()
		self.name = nm

	def pretty_print(self):
		put(self.name)

	def eval(self, context):
		try:
			return context.get_name(self.name)
		except KeyError:
			raise(runtime_error("Unbound variable: %s" % self.name))

class function(expr):
	def __init__(self, params, code, context):
		super().__init__()
		self.params = params
		self.code = code
		self.context = context

	def call(self, args):
		new_context = copy.copy(self.context)
		for index, name in enumerate(self.params):
			try:
				new_context.add_name(name, args[index])
			except IndexError:
				raise runtime_error("Invalid number of arguments")
		return self.code.eval(new_context)

	def eval(self, context):
		return self

	def pretty_print(self):
		put("#{procedure}")
