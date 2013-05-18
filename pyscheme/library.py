import ast

class lambdam(object):
	def call(self, args, context):
		if len(args) != 2:
			raise ast.runtime_error("lambda: lambda takes two arguments")
		params_c = args[0]
		code = args[1]
		if not isinstance(params_c, ast.slist):
			raise ast.runtime_error("lambda: lambda arguments must be in list form")
		params = []
		for elem in params_c.lst:
			if not isinstance(elem, ast.name):
				raise ast.runtime_error("lambda: lambda arguments must be names")
			params.append(elem.name)
		return ast.function(params, code, context)

class definem(object):
	def call(self, args, context):
		if len(args) != 2:
			raise ast.runtime_error("define: define takes two arguments")
		name_c = args[0]
		if not isinstance(name_c, ast.name):
			raise ast.runtime_error("define: variable to be defined must be a name")
		name = name_c.name
		if context.has_name(name):
			raise ast.runtime_error("define: cannot redefine variable %s" % name)
		value = args[1].eval(context)
		context.add_name(name, value)
		return value

class setm(object):
	def call(self, args, context):
		if len(args) != 2:
			raise ast.runtime_error("set!: set! takes two arguments")
		name_c = args[0]
		if not isinstance(name_c, ast.name):
			raise ast.runtime_error("set!: variable to be set must be a name")
		name = name_c.name
		value = args[1].eval(context)
		context.add_name(name, value)
		return value

class printf(object):
	def call(self, args):
		for arg in args:
			arg.pretty_print()
			print()

lib_macros = {
	"lambda": lambdam(),
	"define": definem(),
	"set!": setm()
}

lib_names = {
	"print": printf()
}