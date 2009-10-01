;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
;;;
;;; This file is part of cl-prolog.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(in-package :cl-swi)

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
