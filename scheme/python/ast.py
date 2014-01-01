import copy
import eval

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
		return "'" + self.expr.pretty_print()

	def eval(self, context):
		return self.expr

class literal(expr):
	def __init__(self, tkn, content):
		super().__init__()
		self.tkn = tkn
		self.content = content

	def pretty_print(self):
		if self.tkn == T_STRING:
			return '"' + self.content.replace('"', '\\"') + '"'
		elif self.tkn == T_INT or self.tkn == T_FLOAT:
			return str(self.content)
		elif self.tkn == T_RATIONAL:
			num, denom = self.content
			return str(num) + '/' + str(denom)
		elif self.tkn == T_COMPLEX:
			real, imag = self.content
			return str(real) + '+' + str(imag) + 'i'
		elif self.tkn == T_BOOL:
			if self.content:
				return "#t"
			else:
				return "#f"

	def eval(self, context):
		return self

class slist(expr):
	def __init__(self, lst):
		super().__init__()
		self.lst = lst

	def pretty_print(self):
		return '(' + ' '.join(elem.pretty_print() for elem in self.lst) + ')'

	def eval(self, context):
		if len(self.lst) < 1:
			raise runtime_error("Empty list cannot be eval'ed")
		fn = self.lst[0].eval(context)
		args = self.lst[1:]

		if isinstance(fn, function):
			return fn.call([elem.eval(context) for elem in args])
		else:
			return fn.call(args, context)

class statement_list(object):
	def __init__(self, prgrm):
		super().__init__()
		self.prgrm = prgrm

	def pretty_print(self):
		return ''.join(line.pretty_print() + '\n' for line in self.prgrm)

	def eval(self, context):
		for line in self.prgrm:
			result = line.eval(context)
		return result

class name(expr):
	def __init__(self, nm):
		super().__init__()
		self.name = nm

	def pretty_print(self):
		return self.name

	def eval(self, context):
		try:
			return context.get(self.name)
		except KeyError:
			raise(runtime_error("Unbound variable: %s" % self.name))

def set_params(params, args, context):
	for index, name in enumerate(params):
		if isinstance(name, dotted_name):
			value = slist(args[index:])
			context.set(name.name, value)
			break
		try:
			context.set(name, args[index])
		except IndexError:
			raise runtime_error("Invalid number of arguments")

class function(expr):
	def __init__(self, params, code, context):
		super().__init__()
		self.params = params
		self.code = code
		self.context = context

	def call(self, args):
		new_context = eval.context(parent=self.context, meval_context=self.context.meval_context)
		set_params(self.params, args, new_context)
		return self.code.eval(new_context)

	def eval(self, context):
		return self

	def pretty_print(self):
		return "#{procedure}"

class dotted_name(expr):
	def __init__(self, name):
		self.name = name.name

	def call(self, args):
		raise runtime_error("cannot call dotted_name")

	def pretty_print(self):
		return " . %s" % self.name

# common objects
nil = slist([])
true = literal(T_BOOL, True)
false = literal(T_BOOL, False)

def scm_bool(b):
	if b:
		return true
	else:
		return false
