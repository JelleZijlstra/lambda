import copy
import fractions
import functools

import ast
from lexer import *

class lib_proc(object):
	def get_name(self):
		'''Evil, illegal way to extract the current class's name'''
		return self.__class__.__name__[:-1]

	def ensure_type(self, value, type):
		if not isinstance(value, type):
			msg = "%s: wrong type for value (expected %s)" % (self.get_name(), type.__name__)
			raise(ast.runtime_error(msg))

	def ensure_arg_type(self, args, n, type):
		if not isinstance(args[n], type):
			msg = "%s: wrong type for argument %d (expected %s)" % (self.get_name(), n, type.__name__)
			raise(ast.runtime_error(msg))

	def ensure_args(self, args, n):
		if len(args) != n:
			msg = "%s: %d arguments provided, but %d were expected" % (self.get_name(), len(args), n)
			raise(ast.runtime_error(msg))

	def eval(self, context):
		return self

class lib_macro(lib_proc):
	def pretty_print(self):
		print("#{macro}", end="")

class lambdam(lib_macro):
	def call(self, args, context):
		self.ensure_args(args, 2)
		self.ensure_arg_type(args, 0, ast.slist)
		code = args[1]
		params = []
		for elem in args[0].lst:
			self.ensure_type(elem, ast.name)
			params.append(elem.name)
		return ast.function(params, code, context)

class definem(lib_macro):
	def call(self, args, context):
		self.ensure_args(args, 2)
		self.ensure_arg_type(args, 0, ast.name)
		name = args[0].name
		if context.has_name(name):
			raise ast.runtime_error("define: cannot redefine variable %s" % name)
		value = args[1].eval(context)
		context.add_name(name, value)
		return value

class setm(lib_macro):
	def call(self, args, context):
		self.ensure_args(args, 2)
		self.ensure_arg_type(args, 0, ast.name)
		name = args[0].name
		value = args[1].eval(context)
		context.add_name(name, value)
		return value

class letm(lib_macro):
	def call(self, args, context):
		self.ensure_args(args, 2)
		self.ensure_arg_type(args, 0, ast.slist)
		new_context = copy.copy(context)
		for pair in args[0].lst:
			self.ensure_type(pair, ast.slist)
			name = pair.lst[0]
			self.ensure_type(name, ast.name)
			code = pair.lst[1]
			new_context.add_name(name.name, code.eval(context))
		return args[1].eval(new_context)

class ifm(lib_macro):
	def call(self, args, context):
		self.ensure_args(args, 3)
		condition = args[0].eval(context)
		is_true = not (isinstance(condition, ast.literal) and condition.content == False)
		if is_true:
			return args[1].eval(context)
		else:
			return args[2].eval(context)

class evalm(lib_macro):
	def call(self, args, context):
		self.ensure_args(args, 1)
		return args[0].eval(context).eval(context)

class lib_function(lib_proc):
	def pretty_print(self):
		print("#{procedure}", end="")

class printf(lib_function):
	def call(self, args):
		for arg in args:
			arg.pretty_print()
			print()
		return ast.nil

class carf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		self.ensure_arg_type(args, 0, ast.slist)
		if len(args[0].lst) == 0:
			raise(runtime_error("car: empty list"))
		return args[0].lst[0]

class cdrf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		self.ensure_arg_type(args, 0, ast.slist)
		if len(args[0].lst) == 0:
			raise(runtime_error("cdr: empty list"))
		return ast.slist(args[0].lst[1:])

class consf(lib_function):
	def call(self, args):
		self.ensure_args(args, 2)
		self.ensure_arg_type(args, 1, ast.slist)
		return ast.slist([args[0]] + args[1].lst)

def lcm(x, y):
	return (x * y) / fractions.gcd(x, y)

class plusf(lib_function):
	def call(self, args):
		if len(args) == 0:
			return ast.literal(T_INT, 0)
		else:
			def reducef(x, y):
				self.ensure_type(x, ast.literal)
				self.ensure_type(y, ast.literal)
				if x.tkn != y.tkn:
					raise(runtime_error("+: arguments must have same type"))
				if x.tkn == T_INT or x.tkn == T_FLOAT:
					return ast.literal(T_INT, x.content + y.content)
				elif x.tkn == T_RATIONAL:
					x1, x2 = x.content
					y1, y2 = y.content
					z2 = lcm(x2, y2)
					z1 = x1 * (z2 / x2) + y1 * (z2 / y2)
					return ast.literal(T_RATIONAL, (int(z1), int(z2)))
				elif x.tkn == T_COMPLEX:
					x1, x2 = x.content
					y1, y2 = x.content
					return ast.literal(T_COMPLEX, (x1 + y1, x2 + y2))
			return functools.reduce(reducef, args)

lib_macros = {
	"lambda": lambdam(),
	"define": definem(),
	"set!": setm(),
	"let": letm(),
	"if": ifm(),
	"eval": evalm(),
}

lib_names = {
	"print": printf(),
	"+": plusf(),
	"cons": consf(),
	"car": carf(),
	"cdr": cdrf(),
}
