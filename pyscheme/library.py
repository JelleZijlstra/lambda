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
			if isinstance(elem, ast.dotted_name):
				params.append(elem)
				break
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

class user_defined_macro(lib_macro):
	def __init__(self, params, body, define_context):
		super().__init__()
		self.params = params
		self.body = body
		self.define_context = define_context

	def call(self, args, context):
		new_context = copy.copy(self.define_context)
		new_context.set_meval_context(context)
		ast.set_params(self.params, args, new_context)
		return self.body.eval(new_context)

class defmacrom(lib_macro):
	def call(self, args, context):
		self.ensure_args(args, 3)
		self.ensure_arg_type(args, 0, ast.name)
		name = args[0].name
		self.ensure_arg_type(args, 1, ast.slist)
		params = args[1].lst
		body = args[2]
		macro = user_defined_macro(params, body, context)
		context.add_macro(name, macro)
		return macro

class mevalm(lib_macro):
	def call(self, args, context):
		self.ensure_args(args, 1)
		return args[0].eval(context.get_meval_context())

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

class appendf(lib_function):
	def call(self, args):
		def reducef(x, y):
			self.ensure_type(y, ast.slist)
			return x + y.lst
		return ast.slist(functools.reduce(reducef, args, []))

class gensymf(lib_function):
	counter = 0
	def call(self, args):
		name = "var" + counter
		counter = counter + 1
		return ast.name(name)

class nullf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		self.ensure_arg_type(args, 0, ast.slist)
		return ast.scm_bool(len(args[0].lst) == 0)

class listf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.slist))

class stringf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.literal) and args[0].tkn == T_STRING)

class charf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		# we don't actually have a char type
		return ast.false

class numberf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.literal) and args[0].tkn != T_STRING and args[0].tkn != T_BOOL)

class integerf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.literal) and args[0].tkn == T_INT)

class rationalf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.literal) and (args[0].tkn == T_INT or args[0].tkn == T_RATIONAL))

class realf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.literal) and (args[0].tkn == T_INT or args[0].tkn == T_RATIONAL or args[0].tkn == T_FLOAT))

class complexf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.literal) and args[0].tkn != T_STRING and args[0].tkn != T_BOOL)

class integerf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.literal) and args[0].tkn == T_INT)

class booleanf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.literal) and args[0].tkn == T_BOOL)

class symbolf(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.name))

class proceduref(lib_function):
	def call(self, args):
		self.ensure_args(args, 1)
		return ast.scm_bool(isinstance(args[0], ast.function) or isinstance(args[0], lib_function))

lib_macros = {
	"lambda": lambdam(),
	"define": definem(),
	"set!": setm(),
	"let": letm(),
	"if": ifm(),
	"eval": evalm(),
	"defmacro": defmacrom(),
	"meval": mevalm(),
}

lib_names = {
	"print": printf(),
	"+": plusf(),
	"cons": consf(),
	"car": carf(),
	"cdr": cdrf(),
	"append": appendf(),
	"null?": nullf(),
	"list?": listf(),
	"string?": stringf(),
	"char?": charf(),
	"number?": numberf(),
	"integer?": integerf(),
	"rational?": rationalf(),
	"real?": realf(),
	"complex?": complexf(),
	"boolean?": booleanf(),
	"symbol?": symbolf(),
	"procedure?": proceduref(),
}
