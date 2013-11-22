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

cmdline_parser.add_option("--language", action="store", type="string",
                          dest="language", metavar="LANG",
                          help="The language of file to create. If this " +
                               "is not specified the language is guessed " +
                               "from the file extension.")

cmdline_parser.add_option("--type", action="store", type="string",
                          dest="file_type", metavar="TYPE",
                          help="The type of file to create.")

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
		'rb'        : RubyFileCreator,
		'ruby'      : RubyFileCreator,
		'js'        : JavascriptFileCreator,
		'javascript': JavascriptFileCreator,
		'json'      : JsonFileCreator,
		'erl'       : ErlangFileCreator,
		'erlang'    : ErlangFileCreator,
		'Makefile'  : MakefileFileCreator,
		'md'        : MarkdownFileCreator,
		'markdown'  : MarkdownFileCreator,
		'sh'        : BashFileCreator,
		'bash'      : BashFileCreator,
		'html'      : HtmlFileCreator,
		'css'       : CssFileCreator,
		'c'         : CFileCreator,
		'h'         : HFileCreator,
		'java'      : JavaFileCreator,
		'go'        : GoFileCreator
	}

	if not options.header_title:
		options.header_title = file_name
	language = options.language if options.language else file_extension
	return file_creators.get(language, JavascriptFileCreator)(options)

#---------------------------------------------------------------------------

class FileCreator(object):
	def __init__(self, options):
		self.options = options
		self.header_title = options.header_title

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
	def __init__(self, options):
		super(PythonFileCreator, self).__init__(options)

	def print_header(self, new_file):
		print >> new_file, create_header('#', self.header_title)

#---------------------------------------------------------------------------

class RubyFileCreator(FileCreator):
	def __init__(self, options):
		super(RubyFileCreator, self).__init__(options)

	def print_header(self, new_file):
		print >> new_file, create_header('#', self.header_title)

	def print_body(self, new_file):
		print >> new_file, '''
puts 'Hello, World!'
'''

#---------------------------------------------------------------------------

class JavascriptFileCreator(FileCreator):
	def __init__(self, options):
		super(JavascriptFileCreator, self).__init__(options)

	def print_header(self, new_file):
		header_title = self.header_title
		if new_file.name == 'index.js':
			module = os.getcwd().split('/')[-1]
			header_title = 'index.js for the {0} module'.format(module)
		print >> new_file, create_header('//', header_title)

	def print_footer(self, new_file):

		if new_file.name == 'main.js':
			print >> new_file, '''
require.config({
  paths: {
    'jquery': '../bower_components/jquery/jquery',
    'handlebars': '../bower_components/handlebars/handlebars',
    'text': '../bower_components/requirejs-text/text',
    'hbs': '../bower_components/requirejs-hbs/hbs'
  },

  shim: {
    'handlebars': {
      exports: 'Handlebars'
    }
  }
});

require(['jquery', 'handlebars'], function main($, Handlebars) {
  $(document).ready(function () {
    var name = prompt("What's your name?")
      , template = Handlebars.compile('<h1>Hello, {{name}}!</h1>');

    if (name === '') {
      name = 'World';
    }

    $('body').append(template({ name: name }));
  });
});'''
		else:
			print >> new_file, '''
define(['jquery'], function ($) {
});'''

#---------------------------------------------------------------------------

class JsonFileCreator(FileCreator):
	def __init__(self, options):
		super(JsonFileCreator, self).__init__(options)

	def print_footer(self, new_file):
		project = os.getcwd().split('/')[-1]
		print >> new_file, '''{{
  "name": "{0}",
  "version": "0.0.0",
  "authors": [
    "Martin Ortega <martega6@gmail.com>"
  ],
  "dependencies": {{
    "requirejs-text": "~2.0.10",
    "requirejs-hbs": "*",
    "handlebars": "~1.1.2",
    "jquery": "~2.0.3",
    "requirejs": "~2.1.9"
  }}
}}'''.format(project)

#---------------------------------------------------------------------------

class ErlangFileCreator(FileCreator):
	def __init__(self, header_title):
		super(ErlangFileCreator, self).__init__(header_title)

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
	def __init__(self, options):
		super(MakefileFileCreator, self).__init__(options)

	def print_header(self, new_file):
		project = os.getcwd().split('/')[-1]
		header_title = 'Makefile for ' + project
		print >> new_file, create_header('#', header_title)

	def print_body(self, new_file):
		makefile_types = {
			'js'         : self.print_javascript_body,
			'javascript' : self.print_javascript_body,
			'c'          : self.print_c_body
		}

		makefile_types.get(self.options.file_type, 'js')(new_file)

	def print_javascript_body(self, new_file):
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

	def print_c_body(self, new_file):
		print >> new_file, '''
CC = clang
TARGETS = foo
VPATH = src:obj

foo-obj = main.o

run: $(TARGETS)
	@echo ----------------------------------------------------------------------------
	@./$<

foo: $(foo-obj)
	@echo Building executable $@...
	@cd obj; \\
	$(CC) $+ -o $@; \\
	mv $@ ..

%.o: %.c
	@echo Compiling $<...
	@mkdir -p obj
	@$(CC) $< -std=c11 -c
	@mv $@ obj

clean:
	rm -rf obj
	rm $(TARGETS)

.PHONY: run clean'''

#---------------------------------------------------------------------------

class MarkdownFileCreator(FileCreator):
	def __init__(self, options):
		super(MakefileFileCreator, self).__init__(options)

	def print_body(self, new_file):
		project = os.getcwd().split('/')[-1]
		project = ' '.join([x.title() for x in project.split('-')])
		print >> new_file, '''# {0}

## Introduction

## Setup

## Running'''.format(project)

#---------------------------------------------------------------------------

class BashFileCreator(FileCreator):
	def __init__(self, options):
		super(BashFileCreator, self).__init__(options)

	def print_hash_bang(self, new_file):
		print >> new_file, '#!/usr/bin/env bash'

	def print_header(self, new_file):
		print >> new_file, create_header('#', self.header_title)

#---------------------------------------------------------------------------

class HtmlFileCreator(FileCreator):
	def __init__(self, options):
		super(HtmlFileCreator, self).__init__(options)

	def print_body(self, new_file):
		print >> new_file, '''<!DOCTYPE html>
<html>

<head>
  <title></title>

  <!-- css -->
  <style>
    html, body {
      height: 100%;
      margin: 0px;
    }
  </style>

  <!-- javascript -->
  <script data-main="src/main" src="bower_components/requirejs/require.js"></script>

  </script>
</head>

<body>
</body>

</html>
'''

#---------------------------------------------------------------------------

class CssFileCreator(FileCreator):
	def __init__(self, options):
		super(CssFileCreator, self).__init__(options)

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

class CFileCreator(FileCreator):
	def __init__(self, options):
		super(CFileCreator, self).__init__(options)

	def print_header(self, new_file):
		header_title = self.header_title
		if new_file.name == 'main.c':
			project = os.getcwd().split('/')[-1]
			header_title = 'main.c for the {0} project'.format(project)
		print >> new_file, create_header('//', header_title)

	def print_body(self, new_file):
		if new_file.name == 'main.c':
			print >> new_file, '''
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char** argv)
{
}'''


#---------------------------------------------------------------------------

class HFileCreator(FileCreator):
	def __init__(self, options):
		super(HFileCreator, self).__init__(options)

	def print_header(self, new_file):
		print >> new_file, create_header('//', self.header_title)

	def print_body(self, new_file):
		print >> new_file, '''
#ifndef {0}
#define {0}

#endif'''.format(new_file.name.upper().replace('.', '_'))

#---------------------------------------------------------------------------

class JavaFileCreator(FileCreator):
	def __init__(self, options):
		super(JavaFileCreator, self).__init__(options)

	def print_header(self, new_file):
		header_title = self.header_title
		print >> new_file, create_header('//', header_title)

	def print_body(self, new_file):
		class_name = new_file.name.split('.')[0]
		print >> new_file, '''
public class {0} {{

	public static void main(String[] args) {{
		System.out.println("Hello, World!");
	}}

}}'''.format(class_name)


#---------------------------------------------------------------------------

class GoFileCreator(FileCreator):
	def __init__(self, options):
		super(GoFileCreator, self).__init__(options)

	def print_header(self, new_file):
		header_title = self.header_title
		print >> new_file, create_header('//', header_title)

	def print_body(self, new_file):
		print >> new_file, '''
package main

import "fmt"

func main() {
	fmt.Printf("Hello, World!\\n");
}
'''

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
