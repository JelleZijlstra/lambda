import ast
import copy
import library

class context(object):
	def __init__(self, names, macros):
		super().__init__()
		self.names = copy.copy(names)
		self.macros = copy.copy(macros)

	def copy(self):
		return context(self.names, self.macros)

	def get_name(self, name):
		return self.names[name]

	def get_macro(self, name):
		return self.macros[name]

	def add_macro(self, name, defn):
		self.macros[name] = defn

	def add_name(self, name, defn):
		self.names[name] = defn

	def has_name(self, name):
		return name in self.names

	def has_macro(self, name):
		return name in self.macros

def eval(tree):
	tree.eval(context(library.lib_names, library.lib_macros))
