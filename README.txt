
Introduction

cl-prolog is an experiment in combining Prolog and Lisp so that
various Prolog engines could be embedded in Lisp and programmed using
Lisp syntax. While there exist Prologs that are written in Common
Lisp, the goal here was to access non-Lisp Prolog implementations and
the wide range of libraries they provide.

This project was started when I knew some basic Common Lisp and
nothing at all about Prolog. It should be viewed as proof of concept
code; nothing is guaranteed to work, tests may fail, public interfaces
are subject to change.

That said, it wouldn't take too much effort to pull things together
into a working system, given an incentive.


Installation

cl-prolog uses ASDF for system definition. Copy or symlink
cl-prolog.asd (and optionally cl-prolog-test.asd) to
your asdf:*central-registry* and load cl-prolog with the
asdf:operate function:

 (asdf:operate 'asdf:load-op :cl-prolog)

or with the equivalent deoxybyte-systems:load-system function:
 
 (dxs:load-system :cl-prolog)

In addition, a Prolog backend must be loaded:

 (dxs:load-system :cl-swi-client) ; SWI-Prolog socket backend
 (dxs:load-system :cl-swi)        ; SWI-Prolog FFI backend

The XSB Prolog FFI backend has not been updated for some time and
probably does not work.


Tests

To run the unit and regression tests you need to have LIFT
installed. Run the tests with the asdf:operate function:

 (asdf:operate 'asdf:test-op :cl-prolog)

or with the equivalent deoxybyte-systems:test-system function:

 (dxs:test-system :cl-prolog)


Documentation

See the Lisp docstrings, particularly the package docstrings for an
overview. HTML documentation may be generated with the command:

 (dxs:document-system :cl-prolog)

at the REPL, provided that CLDOC is installed.


Dependencies

deoxybyte-systems       git://github.com/keithj/deoxybyte-systems.git

cl-ppcre                http://weitz.de/cl-ppcre/


Optional dependencies

LIFT                    http://common-lisp.net/project/lift/
CLDOC                   http://common-lisp.net/project/cldoc/
CFFI                    http://common-lisp.net/project/cffi/
