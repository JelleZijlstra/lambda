import ast
import copy
import library
import os.path
import parser

LIBRARY_FILE = os.path.join(os.path.dirname(__file__), "../common/library.scm")

class context(object):
	def __init__(self, parent, meval_context=None):
		super().__init__()
		self.names = {}
		self.parent = parent
		self.meval_context = meval_context

	def get(self, name):
		try:
			return self.names[name]
		except KeyError:
			return self.parent.get(name)

	def set(self, name, defn):
		self.names[name] = defn

	def has(self, name):
		return name in self.names

	@staticmethod
	def new_context():
		ctxt = context(None)
		ctxt.names = library.library
		lib = parser.parse(open(LIBRARY_FILE).read())
		lib.eval(ctxt)
		return ctxt

def eval(tree):
	tree.eval(context.new_context())
