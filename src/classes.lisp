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

(in-package :cl-prolog-sys)

(defclass reference-mixin ()
  ((ref :initarg :ref
        :accessor ref-of
        :documentation "A reference to the Prolog entity."))
  (:documentation "A mixin class providing a slot which contains a
reference or handle to a Prolog entity such as a term, functor or
module."))

(defclass name-mixin ()
  ((name :initform nil
         :initarg :name
         :accessor name-of
         :documentation "The name of the named object."))
  (:documentation "A mixin class providing a slot which contains a
name"))

(defclass prolog ()
  ()
  (:documentation "A Prolog backend."))

(defclass prolog-query ()
  ((prolog :initarg :prolog
           :reader prolog-of
           :documentation "The Prolog to which the query applies."))
  (:documentation "A Prolog query handle. Once a query handle is
obtained, possible answers may be obtained successively from it until
there are not further answers or the handle is relased."))

(defclass term ()
  ((prolog :initarg :prolog
           :reader prolog-of
           :documentation "The Prolog to which the term belongs."))
  (:documentation "A Prolog term."))

(defclass compound-term (term)
  ((functor :initform nil
            :initarg :functor
            :reader functor-of
            :documentation "The functor component of the compound
term.")
   (args :initform nil
         :initarg :args
         :reader args-of
         :documentation "The functor arguments of the compound term."))
  (:documentation "A Prolog compound term."))

(defclass module (name-mixin)
  ()
  (:documentation "A Prolog module."))

(defclass functor (name-mixin)
  ((arity :initarg :arity
          :accessor arity-of
          :documentation "The functor arity."))
  (:documentation "A Prolog functor."))

(defclass predicate ()
  ((functor :initform nil
            :initarg :functor
            :reader functor-of
            :documentation "The predicate functor.")
   (module :initform nil
           :initarg :module
           :reader module-of
           :documentation "The module namespace of the predicate."))
  (:documentation "A Prolog predicate."))
