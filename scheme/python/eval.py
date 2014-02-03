import ast
import copy
import library
import os.path
import parser

LIBRARY_FILE = os.path.join(os.path.dirname(__file__), "../common/library.scm")

class context(object):
	def __init__(self, names, macros, meval_context = None):
		super().__init__()
		self.names = copy.copy(names)
		self.macros = copy.copy(macros)
		self.meval_context = meval_context

	def copy(self):
		return context(self.names, self.macros, meval_context=self.meval_context)

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

	def set_meval_context(self, ctxt):
		self.meval_context = ctxt

	def get_meval_context(self):
		return self.meval_context

	@staticmethod
	def new_context():
		ctxt = context(library.lib_names, library.lib_macros)
		lib = parser.parse(open(LIBRARY_FILE).read())
		lib.eval(ctxt)
		return ctxt

def eval(tree):
	tree.eval(context.new_context())
