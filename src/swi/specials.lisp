
(in-package #:cl-swi)

(defconstant +swi-true+ #x01
  "SWI-Prolog true value.")
(defconstant +swi-false+ #x00
  "SWI-Prolog false value.")

(defconstant +swi-variable+ 1
  "SWI-Prolog variable type identifier.")
(defconstant +swi-atom+ 2
  "SWI-Prolog atom type identifier.")
(defconstant +swi-integer+ 3
  "SWI-Prolog integer type identifier.")
(defconstant +swi-float+ 4
  "SWI-Prolog float type identifier.")
(defconstant +swi-string+ 5
  "SWI-Prolog string type identifier.")
(defconstant +swi-compound-term+ 6
  "SWI-Prolog compond type identifier.")

(defvar *swi-q-normal* #x2
  "Perform a normal query.")
(defvar *swi-q-nodebug* #x4
  "Perform a query with the debugger switched off.")
(defvar *swi-q-catch-exception* #x08
  "Handle exceptions in C.")
(defvar *swi-q-pass-exception* #x10
  "Pass exceptions to calling environment.")

(defvar *swi-cvt-variable* #x20
  "SWI-Prolog conversion flag.")
(defvar *swi-buf-discardable* #x00
  "SWI-Prolog conversion flag.")

(defvar *swi-fa-notrace* #x01
  "SWI-Prolog foreign cannot be traced.")
(defvar *swi-fa-transparent* #x02
  "SWI-Prolog foreign is module transparent.")
(defvar *swi-fa-nondeterministic* #x04
  "SWI-Prolog foreign is non-deterministic.")
(defvar *swi-fa-varargs* #x08
  "SWI-Prolog call using t0, ac, ctx.")