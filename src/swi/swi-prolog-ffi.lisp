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

(in-package :swi-prolog-ffi)

(cffi:define-foreign-library libpl
  (t (:default "libpl")))

(cffi:use-foreign-library libpl)

(defctype atom-t :unsigned-long
  "Prolog atom")
(defctype module-t :pointer
  "Prolog module")
(defctype predicate-t :pointer
  "Prolog procedure")
(defctype term-t :unsigned-long
  "Opaque Prolog term handle")
(defctype qid-t :unsigned-long
  "Opaque Prolog query handle")
(defctype pl-fid-t :unsigned-long
  "Opaque foreign context handle")
(defctype functor-t :unsigned-long
  "Name/arity pair")
(defctype foreign-t :unsigned-long
  "Return type of foreign functions.")


(defcfun ("PL_new_module" pl-new-module) module-t
  (name atom-t))

(defcfun ("PL_open_foreign_frame" pl-open-foreign-frame) pl-fid-t)

(defcfun ("PL_close_foreign_frame" pl-close-foreign-frame) :void
  (cid pl-fid-t))

(defcfun ("PL_register_foreign" pl-register-foreign) :int
  (name :string)
  (arity :int)
  (function :pointer)
  (flag :int)) ;; pointer to C function

(defcfun ("PL_pred" pl-pred) predicate-t
  (f functor-t)
  (m module-t))

(defcfun ("PL_predicate" pl-predicate) predicate-t
  (predicate-name :string) ;; :pointer) ;; const char ptr
  (arity :int)
  (module-name :string)) ;; :pointer)) ;; const char ptr

(defcfun ("PL_open_query" pl-open-query) qid-t
  (m module-t)
  (flags :int)
  (pred predicate-t)
  (t0 term-t))

(defcfun ("PL_next_solution" pl-next-solution) :int
  (qid qid-t))

(defcfun ("PL_close_query" pl-close-query) :void
  (qid qid-t))

(defcfun ("PL_cut_query" pl-cut-query) :void
  (qid qid-t))

(defcfun ("PL_call" pl-call) :int
  (arg-t term-t)
  (m module-t))

(defcfun ("PL_call_predicate" pl-call-predicate) :int
  (m module-t)
  (debug :int)
  (pred predicate-t)
  (t0 term-t))
    
(defcfun ("PL_exception" pl-exception) term-t
  (qid qid-t))

(defcfun ("PL_raise_exception" pl-raise-exception) :int
  (exception term-t))

(defcfun ("PL_throw" pl-throw) :int
  (exception term-t))

(defcfun ("PL_new_term_refs" pl-new-term-refs) term-t
  (n :int))

(defcfun ("PL_new_term_ref" pl-new-term-ref) term-t)

(defcfun ("PL_copy_term_ref" pl-copy-term-ref) term-t
  (from term-t))

(defcfun ("PL_new_atom" pl-new-atom) atom-t
  (s :string)) ;; :pointer))  ;; const char ptr

(defcfun ("PL_atom_chars" pl-atom-chars) :string ;; :pointer ;; const char ptr
  (a atom-t))

(defcfun ("PL_new_functor" pl-new-functor) functor-t
  (f atom-t)
  (a :int))

(defcfun ("PL_get_atom" pl-get-atom) :int
  (arg-t term-t)
  (a :pointer)) ;; atom-t ptr

(defcfun ("PL_get_bool" pl-get-bool) :int
  (arg-t term-t)
  (value :pointer)) ;; int ptr

(defcfun ("PL_get_string" pl-get-string) :int
  (arg-t term-t)
  (s :pointer) ;; const char ptr ptr
  (len :pointer)) ;; unsigned int ptr

(defcfun ("PL_get_chars" pl-get-chars) :int
  (arg-t term-t)
  (s :pointer) ;; unsigned char ptr ptr
  (flags :unsigned-int))

(defcfun ("PL_get_integer" pl-get-integer) :int
  (arg-t term-t)
  (i :pointer)) ;; int ptr

(defcfun ("PL_get_float" pl-get-float) :int
  (arg-t term-t)
  (f :pointer)) ;; double ptr

(defcfun ("PL_get_functor" pl-get-functor) :int
  (arg-t term-t)
  (f :pointer)) ;; functor-t ptr

(defcfun ("PL_get_name_arity" pl-get-name-arity) :int
  (arg-t term-t)
  (name :pointer) ;; atom-t ptr
  (arity :pointer)) ;; int ptr

(defcfun ("PL_get_module" pl-get-module) :int
  (arg-t term-t)
  (module :pointer)) ;; module-t ptr

(defcfun ("PL_get_arg" pl-get-arg) :int
  (index :int)
  (arg-t term-t)
  (a term-t))

(defcfun ("PL_get_list" pl-get-list) :int
  (l term-t)
  (h term-t)
  (arg-t term-t))

(defcfun ("PL_get_head" pl-get-head) :int
  (l term-t)
  (h term-t))

(defcfun ("PL_get_tail" pl-get-tail) :int
  (l term-t)
  (tail term-t))

(defcfun ("PL_get_nil" pl-get-nil) :int
  (l term-t))

(defcfun ("PL_term_type" pl-term-type) :int
  (arg-t term-t))

(defcfun ("PL_is_variable" pl-is-variable) :int
  (arg-t term-t))

(defcfun ("PL_is_ground" pl-is-ground) :int
  (arg-t term-t))

(defcfun ("PL_is_atom" pl-is-atom) :int
  (arg-t term-t))

(defcfun ("PL_is_integer" pl-is-integer) :int
  (arg-t term-t))

(defcfun ("PL_is_string" pl-is-string) :int
  (arg-t term-t))

(defcfun ("PL_is_float" pl-is-float) :int
  (arg-t term-t))

(defcfun ("PL_is_compound" pl-is-compound) :int
  (arg-t term-t))

(defcfun ("PL_is_functor" pl-is-functor) :int
  (arg-t term-t) (f functor-t))

(defcfun ("PL_is_list" pl-is-list) :int
  (arg-t term-t))

(defcfun ("PL_is_atomic" pl-is-atomic) :int
  (arg-t term-t))

(defcfun ("PL_is_number" pl-is-number) :int
  (arg-t term-t))

(defcfun ("PL_put_variable" pl-put-variable) :void
  (arg-t term-t))

(defcfun ("PL_put_atom" pl-put-atom) :void
  (arg-t term-t)
  (a atom-t))

(defcfun ("PL_put_atom_chars" pl-put-atom-chars) :void
  (arg-t term-t)
  (chars :string))

(defcfun ("PL_put_string_chars" pl-put-string-chars) :void
  (arg-t term-t)
  (chars :string))

(defcfun ("PL_put_integer" pl-put-integer) :void
  (arg-t term-t)
  (i :long))

(defcfun ("PL_put_float" pl-put-float) :void
  (arg-t term-t)
  (f :double))

(defcfun ("PL_put_functor" pl-put-functor) :void
  (arg-t term-t)
  (functor functor-t))

(defcfun ("PL_put_list" pl-put-list) :void
  (l term-t))

(defcfun ("PL_put_nil" pl-put-nil) :void
  (l term-t))

(defcfun ("PL_put_term" pl-put-term) :void
  (t1 term-t)
  (t2 term-t))

(defcfun ("PL_cons_functor_v" pl-cons-functor-v) :void
  (h term-t)
  (fd functor-t)
  (a0 term-t))

(defcfun ("PL_cons_list" pl-cons-list) :void
  (l term-t)
  (h term-t)
  (arg-t term-t))

(defcfun ("PL_initialise" pl-initialise) :int
  (argc :int)
  (argv :pointer)) ;; unsigned char ptr ptr

(defcfun ("PL_is_initialised" pl-is-initialised) :int
  (argc :pointer) ;; int ptr
  (argv :pointer)) ;; unsigned char ptr ptr ptr

(defcfun ("PL_cleanup" pl-cleanup) :int
  (status :int))

(defcfun ("PL_halt" pl-halt) :int
  (status :int))

(defcfun ("PL_query" pl-query) :long
  (arg-1 :int))
