#!/usr/bin/env python
############################################################################
#                                  create                                  #
############################################################################

import os
import sys
import optparse

#---------------------------------------------------------------------------

usage          = "create [options] <file-names>"
description    = "Create files with language specific boilerplate already included"
version        = "1.0"
cmdline_parser = optparse.OptionParser(usage=usage,
                                       description=description,
                                       version=version)

cmdline_parser.add_option("--type", action="store", type="string",
                          dest="file_type", metavar="TYPE",
                          help="The type of file to create. If this " +
                               "is not specified the type is guessed " +
                               "from the file extension.")

cmdline_parser.add_option("--title", action="store", type="string",
                          dest="header_title", metavar="TITLE",
                          help="The title to use in generating the " +
                               "file boiler plate.")

#---------------------------------------------------------------------------

def main(argv):
	options, file_names = cmdline_parser.parse_args(argv)

	for name in file_names:
		file_creator = create_file_creator(options, name)
		file_creator.create_file(name)
		print "Created {0}!".format(name)

#---------------------------------------------------------------------------

def create_file_creator(options, file_name):
	file_extension = file_name.split('.')[-1]

	file_creators = {
		'py'        : PythonFileCreator,
		'python'    : PythonFileCreator,
		'js'        : JavascriptFileCreator,
		'javascript': JavascriptFileCreator,
		'erl'       : ErlangFileCreator,
		'erlang'    : ErlangFileCreator,
		'Makefile'  : MakefileFileCreator,
		'md'        : MarkdownFileCreator,
		'markdown'  : MarkdownFileCreator,
		'sh'        : BashFileCreator,
		'bash'      : BashFileCreator,
		'html'      : HtmlFileCreator,
		'css'       : CssFileCreator
	}

	header_title = options.header_title if options.header_title else file_name
	file_type = options.file_type if options.file_type else file_extension
	return file_creators.get(file_type, JavascriptFileCreator)(header_title)

#---------------------------------------------------------------------------

class FileCreator(object):
	def __init__(self, header_title):
		self.header_title = header_title

	def create_file(self, name):
		new_file = open(name, 'w+')
		self.print_hash_bang(new_file)
		self.print_header(new_file)
		self.print_body(new_file)
		self.print_footer(new_file)
		new_file.close()

	def print_hash_bang(self, new_file):
		pass

	def print_header(self, new_file):
		pass

	def print_body(self, new_file):
		pass

	def print_footer(self, new_file):
		pass

#---------------------------------------------------------------------------

class PythonFileCreator(FileCreator):
	def __init__(self, header_title):
		super(PythonFileCreator, self).__init__(self, header_title)

	def print_header(self, new_file):
		print >> new_file, create_header('#', self.header_title)

#---------------------------------------------------------------------------

class JavascriptFileCreator(FileCreator):
	def __int__(self, header_title):
		super(JavascriptFileCreator, self).__init__(self, header_title)

	def print_header(self, new_file):
		header_title = self.header_title
		if new_file.name == 'index.js':
			module = os.getcwd().split('/')[-1]
			header_title = 'index.js for the {0} module'.format(module)
		print >> new_file, create_header('//', header_title)

	def print_footer(self, new_file):
		print >> new_file, '''
//--------------------------------------------------------------------------
// exports

module.exports = {
};'''

#---------------------------------------------------------------------------

class ErlangFileCreator(FileCreator):
	def __init__(self, header_title):
		super(ErlangFileCreator, self).__init__(self, header_title)

	def print_header(self, new_file):
		print >> new_file, create_header('%', self.header_title)

	def print_body(self, new_file):
		module_name = new_file.name.split('.')[0]
		print >> new_file, '''
-module({0}).
-export([
]).'''.format(module_name)

#---------------------------------------------------------------------------

class MakefileFileCreator(FileCreator):
	def __init__(self, header_title):
		super(MakefileFileCreator, self).__init__(self, header_title)

	def print_header(self, new_file):
		project = os.getcwd().split('/')[-1]
		header_title = 'Makefile for ' + project
		print >> new_file, create_header('#', header_title)

	def print_body(self, new_file):
		print >> new_file, '''
MOCHA = ./node_modules/mocha/bin/mocha

test:
	@$(MOCHA) -R spec

test-nyan:
	@$(MOCHA) -R nyan

test-md:
	@$(MOCHA) -R markdown > spec.md

run:
	@node ./src/main.js

.PHONY: run test test-md'''

#---------------------------------------------------------------------------

class MarkdownFileCreator(FileCreator):
	def __init__(self, header_title):
		super(MakefileFileCreator, self).__init__(self, header_title)

	def print_body(self, new_file):
		project = os.getcwd().split('/')[-1]
		project = ' '.join([x.title() for x in project.split('-')])
		print >> new_file, '''# {0}

## Introduction

## Setup

## Running'''.format(project)

#---------------------------------------------------------------------------

class BashFileCreator(FileCreator):
	def __init__(self, header_title):
		super(BashFileCreator, self).__init__(self, header_title)

	def print_hash_bang(self, new_file):
		print >> new_file, '#!/usr/bin/env bash'

	def print_header(self, new_file):
		print >> new_file, create_header('#', self.header_title)

#---------------------------------------------------------------------------

class HtmlFileCreator(FileCreator):
	def __init__(self, header_title):
		super(HtmlFileCreator, self).__init__(header_title)

	def print_body(self, new_file):
		print >> new_file, '''<!DOCTYPE html>
<html>

<head>
	<title></title>

	<!-- stylesheets -->
	<link rel="stylesheet" type="text/css" href="style.css">

	<!-- jQuery -->
	<script src="http://code.jquery.com/jquery-1.10.1.min.js"></script>

	<!-- javascript -->

	<!-- start script -->
	<script>
		$(document).ready(function () {
			alert('You still need to implement the start script!');
		});
	</script>
</head>

<body>
</body>

</html>
'''

#---------------------------------------------------------------------------

class CssFileCreator(FileCreator):
	def __init__(self, header_title):
		super(CssFileCreator, self).__init__(header_title)

	def print_header(self, new_file):
		print >> new_file, '''/*
* {0}
*/
'''.format(self.header_title)

	def print_body(self, new_file):
		print >> new_file, '''
* {
	margin  : 0;
	padding : 0;
	box-sizing      : border-box;
	-moz-box-sizing : border-box;
}

h1, h2, h3, h4, h5, h6, p {
	margin-bottom: 10px;
}

ol, ul, dl {
	list-style-position: inside;
}

body {
	font  : 13px 'Trebuchet MS', Verdana, Helvetica, Arial, sans-serif;
	color : #444;
	background-color : #888;
}

a {
	text-decoration: none;
}

a:link, a:visited {
	color: inherit;
}

strong {
	font-weight : 800;
	color       : #000;
}'''

#---------------------------------------------------------------------------

def create_header(comment, title):
	width = 76
	header = (comment * width)[:width] + '\n'
	line_len = width - len(comment)*2 - 2
	for line in group(title, line_len):
		left_padding = (line_len - len(line))/2
		right_padding = left_padding
		if (line_len - len(line)) % 2 != 0:
			right_padding += 1
		header += comment + ' ' + \
		          ' ' * left_padding + \
		          line + \
		          ' ' * right_padding + \
		          ' ' + comment + '\n'
	header += (comment * width)[:width]
	return header

#---------------------------------------------------------------------------

def group(iterable, size):
	for start in xrange(0, len(iterable), size):
		yield iterable[start:start + size]

#---------------------------------------------------------------------------

if __name__ == '__main__':
	main(sys.argv[1:])
