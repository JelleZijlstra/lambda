#!/opt/local/bin/python3.3

import sys

import eval
import parser

if __name__ == '__main__':
	file = sys.argv[1]
	content = open(file).read()
	program = parser.parse(content)
	eval.eval(program)
