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

(in-package :cl-user)

(defpackage :cl-prolog-sys
  (:use #:common-lisp)
  (:export
   ;; Conditions
   #:prolog-error

   ;; Functions
   #:pl-atom-p
   #:pl-variable-p
   #:anon-pl-variable-p
   #:pl-atom-name
   #:pl-variable-name
   #:pl-functor
   #:pl-arity
   #:pl-quote-atom-p
   #:pl-name-to-lisp
   #:lisp-name-to-pl
   #:proper-list-p
   #:dotted-pair-p
   #:dotted-list-head
   #:dotted-list-tail
   #:pl-expr-p
   #:apply-expression

   ;; Classes
   #:prolog
   #:prolog-query
   #:reference-mixin
   #:name-mixin
   #:term
   #:compound-term
   #:functor
   #:predicate
   #:module

   ;; Slots
   #:functor
   #:name
   #:arity
   #:ref
   #:module

   ;; Generic functions
   #:name-of
   #:ref-of
   #:functor-of
   #:arity-of
   #:args-of
   #:module-of
   #:prolog-of
   #:prolog-type
   #:prolog-state
   #:create-prolog
   #:destroy-prolog
   #:enabled-p
   #:create-compound-term
   #:create-rule
   #:find-prolog-module
   #:call-prolog
   #:prolog-query
   #:open-prolog-query
   #:prolog-next-solution
   #:cut-prolog-query
   #:close-prolog-query)
  (:documentation "Prolog system interface."))

(defpackage :cl-prolog
  (:use #:common-lisp #:cl-prolog-sys)
  (:nicknames #:pro)
  (:export
   ;; Specials
   #:*current-prolog*
   #:*current-module*

   ;; Conditions
   #:prolog-error
   #:prolog-comm-error
   #:prolog-server-error
   #:prolog-client-error

   ;; Interface
   #:start-prolog
   #:stop-prolog
   #:enabled-p
   #:find-module
   #:<-
   #:rule
   #:call
   #:query
   #:open-query
   #:next-solution
   #:cut-query
   #:close-query

   #:defunctor
   #:defpredicate
   #:with-prolog
   #:with-module

   #:pl-atom-p
   #:pl-variable-p
   #:anon-pl-variable-p
   #:pl-atom-name
   #:pl-variable-name

   ;; Predicates
   #:and/2
   #:and/n
   #:or/2
   #:or/n
   #:consult/1
   #:assert/1
   #:retractall/1
   #:=/2
   #:==/2
   #:not==/2
   #:is/2
   #:member/2
   #:findall/3
   #:bagof/3
   #:setof/3)
  (:documentation "Prolog user interface."))
