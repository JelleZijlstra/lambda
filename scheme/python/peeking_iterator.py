class peeking_iterator(object):
	def __init__(self, it, end_default = None):
		self.it = it
		self.have_next = False
		self.end_default = end_default
		self.has_ended = False

	def __iter__(self):
		return self

	def __get_next(self):
		try:
			return self.it.__next__()
		except StopIteration:
			self.has_ended = True
			return self.end_default

	def next(self):
		return self.__next__()

	def __next__(self):
		if self.have_next:
			self.have_next = False
			return self.next_obj
		else:
			return self.__get_next()

	def peek(self):
		if not self.have_next:
			self.next_obj = self.__get_next()
			self.have_next = True
		return self.next_obj

	def has_next(self):
		return not (self.has_ended and (not self.have_next))
