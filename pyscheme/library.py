import ast
import copy

class lib_macro(object):
	def get_name(self):
		'''Evil, illegal way to extract the current class's name'''
		return self.__class__.__name__[:-1]

	def ensure_args(self, args, n):
		if len(args) != n:
			msg = "%s: %d arguments provided, but %d were expected" % (self.get_name(), n, len(args))
			raise(ast.runtime_error(msg))

	def ensure_arg_type(self, args, n, type):
		if not isinstance(args[n], type):
			msg = "%s: wrong type for argument %d (expected %s)" % (self.get_name(), n, type.__name__)
			raise(ast.runtime_error(msg))

	def ensure_type(self, value, type):
		if not isinstance(value, type):
			msg = "%s: wrong type for value (expected %s)" % (self.get_name(), type.__name__)
			raise(ast.runtime_error(msg))

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

class printf(object):
	def call(self, args):
		for arg in args:
			arg.pretty_print()
			print()
		return ast.nil

lib_macros = {
	"lambda": lambdam(),
	"define": definem(),
	"set!": setm(),
	"let": letm(),
	"if": ifm(),
}

lib_names = {
	"print": printf()
}