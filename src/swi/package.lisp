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

(defpackage :swi-prolog-cffi
  (:use #:common-lisp #:cffi)
  (:documentation "SWI-Prolog foreign library interface.")
  (:export
   ;; Typedefs
   #:atom-t #:module-t #:predicate-t #:term-t #:qid-t #:pl-fid-t #:functor-t
   #:foreign-t

   ;; Module functions
   #:pl-new-module

   ;; Frame functions
   #:pl-open-foreign-frame
   #:pl-close-foreign-frame

   ;; Foreign functions
   #:pl-register-foreign

   ;; Predicate functions
   #:pl-pred
   #:pl-predicate

   ;; Prolog interaction functions
   #:pl-open-query
   #:pl-next-solution
   #:pl-close-query
   #:pl-cut-query
   #:pl-call
   #:pl-call-predicate

   ;; Exception functions
   #:pl-exception
   #:pl-raise-exception
   #:pl-throw

   ;; Term reference functions
   #:pl-new-term-refs
   #:pl-new-term-ref
   #:pl-copy-term-ref

   ;; Atom functions
   #:pl-new-atom
   #:pl-atom-chars

   ;; Functor functions
   #:pl-new-functor

   ;; Functions for reading from terms
   #:pl-get-atom
   #:pl-get-bool
   #:pl-get-atom-chars
   #:pl-get-string
   #:pl-get-chars
   #:pl-get-integer
   #:pl-get-long
   #:pl-get-float
   #:pl-get-functor
   #:pl-get-name-arity
   #:pl-get-module
   #:pl-get-arg
   #:pl-get-list
   #:pl-get-head
   #:pl-get-tail
   #:pl-get-nil

   ;; Term type functions
   #:pl-term-type
   #:pl-is-variable
   #:pl-is-ground
   #:pl-is-atom
   #:pl-is-integer
   #:pl-is-string
   #:pl-is-float
   #:pl-is-compound
   #:pl-is-functor
   #:pl-is-list
   #:pl-is-atomic
   #:pl-is-number

   ;; Term assignment functions
   #:pl-put-variable
   #:pl-put-atom
   #:pl-put-atom-chars
   #:pl-put-string-chars
   #:pl-put-integer
   #:pl-put-pointer
   #:pl-put-float
   #:pl-put-functor
   #:pl-put-list
   #:pl-put-nil
   #:pl-put-term

   ;; Compound term functions
   #:pl-cons-functor-v
   #:pl-cons-list

   ;; Unification functions

   ;; Misc functions

   ;; Startup/shutdown functions
   #:pl-initialise
   #:pl-is-initialised
   #:pl-cleanup
   #:pl-halt
   #:pl-query))

(defpackage :cl-swi
  (:use #:common-lisp #:cl-prolog #:cl-prolog-sys #:swi-prolog-cffi)
  (:nicknames "swi")
  (:documentation "SWI-Prolog implementation."))
