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

(in-package :xsb-prolog)

;;; Derived from file : "cell_def_xsb.h"

(fli:define-c-typedef (cell (:foreign-name "Cell")) (:unsigned :long))

;;; Derived from file : "basictypes.h"

(fli:define-c-typedef (prolog-int (:foreign-name "prolog_int")) :int)
(fli:define-c-typedef (prolog-float (:foreign-name "prolog_float"))
                      :double)
(fli:define-c-typedef (reg-num (:foreign-name "reg_num")) :int)
(fli:define-c-typedef (prolog-term (:foreign-name "prolog_term"))
                      (:unsigned :long))
(fli:define-c-typedef (xsbbool (:foreign-name "xsbBool")) :short)
(fli:define-c-typedef (xsbbyte (:foreign-name "byte")) (:unsigned :char))
(fli:define-c-typedef (counter (:foreign-name "counter"))
                      (:unsigned :int))
(fli:define-c-typedef (word (:foreign-name "word")) (:unsigned :long))
(fli:define-c-typedef (pb (:foreign-name "pb")) (:pointer xsbbyte))
(fli:define-c-typedef (pw (:foreign-name "pw")) (:pointer word))
(fli:define-c-typedef (pfi (:foreign-name "PFI"))
                      (:pointer (:function nil :int)))

;;; Derived from file : "varstring_xsb.h"

;(fli:define-c-struct (varstr
;                      (:foreign-name "varstr")
;                      (:forward-reference t)))
(fli:define-c-typedef (varstring (:foreign-name "VarString"))
                      (:struct varstr))
(fli:define-c-struct (varstr-ops (:foreign-name "varstr_ops"))
                     (set (:pointer
                           (:function
                            ((:pointer varstring) (:pointer :char))
                            :void)))
                     (setv
                      (:pointer
                       (:function
                        ((:pointer varstring) (:pointer varstring))
                        :void)))
                     (append (:pointer
                              (:function
                               ((:pointer varstring) (:pointer :char))
                               :void)))
                     (prepend
                      (:pointer
                       (:function
                        ((:pointer varstring) (:pointer :char))
                        :void)))
                     (appendv
                      (:pointer
                       (:function
                        ((:pointer varstring) (:pointer varstring))
                        :void)))
                     (prependv
                      (:pointer
                       (:function
                        ((:pointer varstring) (:pointer varstring))
                        :void)))
                     (compare
                      (:pointer
                       (:function
                        ((:pointer varstring) (:pointer varstring))
                        :int)))
                     (strcmp
                      (:pointer
                       (:function
                        ((:pointer varstring) (:pointer :char))
                        :int)))
                     (appendblk
                      (:pointer
                       (:function
                        ((:pointer varstring) (:pointer :char) :int)
                        :void)))
                     (prependblk
                      (:pointer
                       (:function
                        ((:pointer varstring) (:pointer :char) :int)
                        :void)))
                     (null-terminate
                      (:pointer
                       (:function ((:pointer varstring)) :void)))
                     (ensure-size
                      (:pointer
                       (:function ((:pointer varstring) :int) :void)))
                     (shrink
                      (:pointer
                       (:function ((:pointer varstring) :int) :void)))
                     (destroy
                      (:pointer
                       (:function ((:pointer varstring)) :void))))
(fli:define-c-struct (varstr (:foreign-name "varstr"))
                     (size :int)
                     (increment :int)
                     (length :int)
                     ;(string (:pointer :char))
                     (string (:pointer (:unsigned :char)))
                     (op (:pointer (:struct varstr-ops))))
(fli:define-foreign-function (varstring-init "varstring_init" :source)
                             ((vstr (:pointer varstring)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (varstring-create
                              "varstring_create"
                              :source)
                             ((vstr (:pointer (:pointer varstring))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-variable (varstrops "VarStrOps" :source)
                             :type
                             (:struct varstr-ops))

;;; Derived from file : "cinterf.h"

(fli:define-foreign-function (ptoc-int "ptoc_int" :source)
                             ((arg-1 reg-num))
                             :result-type
                             prolog-int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ptoc-float "ptoc_float" :source)
                             ((arg-1 reg-num))
                             :result-type
                             prolog-float
                             :language
                             :ansi-c)
(fli:define-foreign-function (ptoc-string "ptoc_string" :source)
                             ((arg-1 reg-num))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ptoc-longstring
                              "ptoc_longstring"
                              :source)
                             ((arg-1 reg-num))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ptoc-abs "ptoc_abs" :source)
                             ((arg-1 reg-num))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ctop-int "ctop_int" :source)
                             ((arg-1 reg-num) (arg-2 prolog-int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ctop-float "ctop_float" :source)
                             ((arg-1 reg-num) (arg-2 :double))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ctop-string "ctop_string" :source)
                             ((arg-1 reg-num) (arg-2 (:pointer :char)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (ctop-abs "ctop_abs" :source)
                             ((arg-1 reg-num) (arg-2 (:pointer :char)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (string-find "string_find" :source)
                             ((arg-1 (:pointer :char)) (arg-2 :int))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (ctop-term "ctop_term" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 (:pointer :char))
                              (arg-3 reg-num))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (ptoc-term "ptoc_term" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 (:pointer :char))
                              (arg-3 reg-num))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (reg-term "reg_term" :source)
                             ((arg-1 reg-num))
                             :result-type
                             prolog-term
                             :language
                             :ansi-c)
(fli:define-foreign-function (c2p-int "c2p_int" :source)
                             ((arg-1 prolog-int) (arg-2 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (c2p-float "c2p_float" :source)
                             ((arg-1 :double) (arg-2 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (c2p-string "c2p_string" :source)
                             ;((arg-1 (:pointer :char))
                              ((arg-1 (:reference-pass :ef-mb-string))
                              (arg-2 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (c2p-list "c2p_list" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (c2p-nil "c2p_nil" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (c2p-functor "c2p_functor" :source)
                              ;((arg-1 (:pointer :char))
                              ((arg-1 (:reference-pass :ef-mb-string))
                              (arg-2 :int)
                              (arg-3 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (c2p-setfree "c2p_setfree" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (c2p-chars "c2p_chars" :source)
                             ((str (:pointer :char))
                              (term prolog-term))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2c-int "p2c_int" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             prolog-int
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2c-float "p2c_float" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             prolog-float
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2c-string "p2c_string" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             ; (:pointer :char)
                             (:pointer (:unsigned :char))
                             ; (:reference-pass :ef-mb-string)
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2c-functor "p2c_functor" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             ; (:pointer :char)
                             (:pointer (:unsigned :char))
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2c-arity "p2c_arity" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2c-chars "p2c_chars" :source)
                             ((arg-1 prolog-term)
                              (arg-2 (:pointer :char))
                              (arg-3 :int))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2p-arg "p2p_arg" :source)
                             ((arg-1 prolog-term) (arg-2 :int))
                             :result-type
                             prolog-term
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2p-car "p2p_car" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             prolog-term
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2p-cdr "p2p_cdr" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             prolog-term
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2p-new "p2p_new" :source)
                             nil
                             :result-type
                             prolog-term
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2p-unify "p2p_unify" :source)
                             ((arg-1 prolog-term) (arg-2 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2p-call "p2p_call" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2p-funtrail "p2p_funtrail" :source)
                             nil
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2p-deref "p2p_deref" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             prolog-term
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-var "is_var" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-int "is_int" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-float "is_float" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-string "is_string" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-atom "is_atom" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-list "is_list" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-nil "is_nil" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-functor "is_functor" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-charlist "is_charlist" :source)
                             ((arg-1 prolog-term)
                              (arg-2 (:pointer :int)))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (is-attv "is_attv" :source)
                             ((arg-1 prolog-term))
                             :result-type
                             xsbbool
                             :language
                             :ansi-c)
(fli:define-foreign-function (c2p-term "c2p_term" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 (:pointer :char))
                              (arg-3 prolog-term))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (p2c-term "p2c_term" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 (:pointer :char))
                              (arg-3 prolog-term))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-c-typedef (vfile (:foreign-name "vfile")) (:pointer :char))
(fli:define-foreign-function (vfile-open "vfile_open" :source)
                             nil
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (vfile-obj "vfile_obj" :source)
                             nil
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-init "xsb_init" :source)
                             ((arg-1 :int)
                              ;(arg-2 (:pointer (:pointer :char))))
                              (arg-2 (:pointer (:pointer (:unsigned :char)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-init-string
                              "xsb_init_string"
                              :source)
                             ;((arg-1 (:pointer :char)))
                             ((arg-1 (:reference-pass :ef-mb-string)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-command "xsb_command" :source)
                             nil
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-command-string
                               "xsb_command_string"
                               :source)
                              ;((arg-1 (:pointer :char)))
                              ((arg-1 (:reference-pass :ef-mb-string)))
                              :result-type
                              :int
                              :language
                              :ansi-c)
(fli:define-foreign-function (xsb-query "xsb_query" :source)
                             nil
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-query-string
                              "xsb_query_string"
                              :source)
                             ;;((arg-1 (:pointer (:unsigned :char))))
                             ((arg-1 (:reference-pass :ef-mb-string)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-query-string-string
                              "xsb_query_string_string"
                              :source)
                             ;((arg-1 (:pointer :char))
                             ((arg-1 (:pointer (:unsigned :char)))
                              (arg-2 (:pointer varstring))
                              ;(arg-3 (:pointer :char)))
                              (arg-3 (:pointer (:unsigned :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-query-string-string-b
                              "xsb_query_string_string_b"
                              :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 (:pointer :char))
                              (arg-3 :int)
                              (arg-4 (:pointer :int))
                              (arg-5 (:pointer :char)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-next "xsb_next" :source)
                             nil
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-next-string
                              "xsb_next_string"
                              :source)
                             ((arg-1 (:pointer varstring))
                              ;(arg-2 (:pointer :char)))
                              (arg-2 (:pointer (:unsigned :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-next-string-b
                              "xsb_next_string_b"
                              :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 :int)
                              (arg-3 (:pointer :int))
                              (arg-4 (:pointer :char)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-get-last-answer-string
                              "xsb_get_last_answer_string"
                              :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 :int)
                              (arg-3 (:pointer :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-close-query
                              "xsb_close_query"
                              :source)
                             nil
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (xsb-close "xsb_close" :source)
                             nil
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (print-pterm "print_pterm" :source)
                             ((arg-1 cell)
                              (arg-2 :int)
                              (arg-3 (:pointer varstring)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (p-charlist-to-c-string
                              "p_charlist_to_c_string"
                              :source)
                             ((term prolog-term)
                              (buf (:pointer varstring))
                              (in-func (:pointer :char))
                              (where (:pointer :char)))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
