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

(in-package :cl-xsb)

(defconstant +xsb-true+ 0
  "XSB Prolog true value.")
(defconstant +xsb-false+ 1
  "XSB Prolog false value.")

(defparameter *xsb-library*
  "/usr/local/src/xsb-2.7.1/config/i686-pc-linux-gnu/bin/libxsb.so")

(defparameter *vars->terms* nil
  "Map of symbols representing Prolog variables in a query to terms in
the Prolog environment.")
(defparameter *terms->vars* nil
  "Map of terms in the Prolog environment to symbols representing
Prolog variables.")

(fli:register-module "libxsb.so"
                     :real-name *xsb-library*
                     :connection-style :immediate)

(defun pl-init (arg-list)
  (zerop (xsb-init (length arg-list) (create-foreign-argv arg-list))))

(defun create-foreign-argv (arg-list)
  (let ((argv (fli:allocate-foreign-object
               :type '(:pointer (:unsigned :char))
               :nelems (length arg-list))))
    (fli:with-coerced-pointer (ptr) argv
      (dotimes (n (length arg-list))
        (setf (fli:dereference ptr)
              (fli:convert-to-foreign-string (nth n arg-list)))
        (fli:incf-pointer ptr)))
    argv))

(defclass xsb-prolog (prolog)
  ())

(defclass xsb-query (query)
  ())

(defclass xsb-term (term reference-mixin)
  ())

(defclass xsb-compound-term (xsb-term compound-term)
  ())

(defclass xsb-module (module)
  ())

(defmethod print-object ((obj xsb-term) stream)
  (with-slots (ref) obj
    (format stream "<XSB-TERM ref: ~A>"
            ref)))

(defmethod print-object ((obj xsb-compound-term) stream)
  (with-slots (ref) obj
    (format stream "<XSB-COMPOUND-TERM ref: ~A>"
            ref)))

(defmethod print-object ((obj xsb-module) stream)
  (with-slots (name) obj
    (format stream "<XSB-MODULE name: ~A>"
            name)))

(defmethod write-term ((obj xsb-term) (arg symbol))
  (if (pl-variable-p arg)
      (ensure-variable arg obj)
    (progn
      (c2p-string (pl-atom-name arg) (ref obj))
      (intern (pl-atom-name arg))))
  obj)

(defmethod write-term ((obj xsb-term) (arg integer))
  (c2p-int arg (ref obj))
  obj)

(defmethod write-term ((obj xsb-term) (arg float))
  (c2p-float arg (ref obj))
  obj)

(defmethod write-term ((obj xsb-term) (arg string))
  (c2p-chars arg (ref obj))
  obj)

(defmethod write-term ((obj xsb-term) (arg xsb-term))
  (p2p-unify (ref obj) (ref arg))
  obj)

(defmethod write-term ((obj xsb-term) (arg (eql nil)))
  (c2p-nil (ref obj))
  obj)

(defmethod write-term ((obj xsb-term) (arg cons))
  (cond ((dotted-pair-p arg)
         (c2p-list (ref obj))
         (let ((head (make-instance 'xsb-term :ref (p2p-car (ref obj))))
               (tail (make-instance 'xsb-term :ref (p2p-cdr (ref obj)))))
           (write-term head (car arg))
           (write-term tail (cdr arg))))
        ((proper-list-p arg)
         (c2p-list (ref obj))
         (let ((head (make-instance 'xsb-term :ref (p2p-car (ref obj))))
               (tail-ref (p2p-cdr (ref obj))))
           (dolist (elt (butlast arg))
             (write-term head elt)
             (c2p-list tail-ref)
             (setf head (make-instance 'xsb-term :ref (p2p-car tail-ref)))
             (setf tail-ref (p2p-cdr tail-ref)))
           (write-term head (car (last arg)))
           (c2p-nil tail-ref)))
        (t
         (c2p-list (ref obj))
         (let ((head (make-instance 'xsb-term :ref (p2p-car (ref obj))))
               (tail-ref (p2p-cdr (ref obj))))
           (dolist (elt (butlast (dotted-list-head arg)))
             (write-term head elt)
             (c2p-list tail-ref)
             (setf head (make-instance 'xsb-term :ref (p2p-car tail-ref)))
             (setf tail-ref (p2p-cdr tail-ref)))
           (write-term head (car (last arg)))
           (write-term (make-instance 'xsb-term :ref tail-ref)
                       (dotted-list-tail arg)))))
  obj)

(defmethod read-term ((obj xsb-term))
  (let ((term-ref (ref obj)))
    (cond ((= 1 (is-functor term-ref))
           (read-functor obj))
          ((= 1 (is-string term-ref))
           (read-atom obj))
          ((= 1 (is-int term-ref))
           (read-integer obj))
          ((= 1 (is-float term-ref))
           (read-float obj))
          ((= 1 (is-var term-ref))
           "string")
          ((= 1 (is-list term-ref))
           (read-list obj))
          ((= 1 (is-nil term-ref))
           "FIXME nil")
          (t (error "Unknown term type")))))

(defmethod cl-prolog-sys:start-prolog ((args list)
                                       (prolog-type (eql :xsb-prolog)))
  (if (pl-init args)
      (make-instance 'xsb-prolog
                     :state :started
                     :prolog-type prolog-type)
    (error "Failed to start XSB Prolog")))

(defmethod cl-prolog-sys:create-compound-term ((name string) (args list)
                                               (prolog xsb-prolog))
  (let ((arity (length args))
        (ref (p2p-new)))
    (c2p-functor name arity ref)
    (let ((terms (loop for arg from 1 to arity
                       collect (make-instance 'xsb-term
                                              :ref (p2p-arg ref arg)))))
      (mapc #'write-term terms args)
      (make-instance 'xsb-compound-term :ref ref :args terms))))

(defmethod cl-prolog-sys:call-prolog ((expr list) module
                                      (prolog xsb-prolog))
  (let ((*vars->terms* (make-hash-table))
        (*terms->vars* (make-hash-table)))
    (let ((term (apply-expression expr)))
      (xsb-true-p (call-xsb-prolog term)))))

(defmethod cl-prolog-sys:query-prolog ((expr list) module
                                       (prolog xsb-prolog))
  (let ((*vars->terms* (make-hash-table))
        (*terms->vars* (make-hash-table)))
    (let ((bindings nil)
          (result nil))
      (do ((solution (open-xsb-query (apply-expression expr)) (xsb-next)))
          ((xsb-false-p solution))
        (when (null result)
          (setf result t))
        (maphash #'(lambda (term var)
                     (let ((deref (make-instance 'xsb-term
                                                 :ref (p2p-deref (ref term)))))
                       (push (cons var (read-term deref)) bindings)))
                 *terms->vars*))
      (xsb-close-query)
      (values bindings result))))

(defun xsb-true-p (arg)
  "Returns T if ARG is the XSB Prolog true value, otherwise NIL."
  (= +xsb-true+ arg))

(defun xsb-false-p (arg)
  "Returns T if ARG is the XSB Prolog false value, otherwise NIL."
  (= +xsb-false+ arg))

(defun ensure-variable (symbol term)
   "Associates SYMBOL with a free Prolog variable TERM. If SYMBOL is
already associated with a Prolog variable, the cached value is
returned."
  (unless (anon-pl-variable-p symbol)
    (multiple-value-bind (value cached)
        (gethash symbol *vars->terms*)
      (if cached
          (progn
            (p2p-unify (ref term) (ref value))
            value)
        (progn
          (setf (gethash term *terms->vars*) symbol)
          (setf (gethash symbol *vars->terms*) term))))))

(defun read-variable (term)
  "Reads a Lisp string representation of an unbound variable from
TERM."
  (fli:convert-from-foreign-string
   (p2c-string (ref term)) :null-terminated-p t))

(defun read-atom (term)
  "Reads a Lisp symbol from TERM."
  (let ((atom-name (fli:convert-from-foreign-string
                    (p2c-string (ref term)) :null-terminated-p t)))
    (if (string= "[]" atom-name)
        '(nil)
      (intern atom-name))))

(defun read-integer (term)
  "Reads a Lisp integer from TERM."
  (p2c-int (ref term)))

(defun read-float (term)
  "Reads a Lisp float from TERM."
  (p2c-float (ref term)))

(defun read-functor (term)
  "Reads a Prolog functor and arguments from TERM."
  (let ((name (fli:convert-from-foreign-string
               (p2c-functor (ref term)) :null-terminated-p t))
        (arity (p2c-arity (ref term))))
    (cons (intern name) (read-args term arity))))
  
(defun read-args (term arity)
  "Reads a Lisp list of arguments of a Prolog functor of ARITY."
  (let ((functor-ref (ref term)))
    (loop for arg from 1 to arity
          collect (read-term (make-instance 'xsb-term
                                            :ref (p2p-arg functor-ref arg))))))

(defun read-list (term)
  "Reads a Lisp list from TERM."
  (if (zerop (is-list (p2p-cdr (ref term))))
      (read-term (make-instance 'xsb-term :ref (p2p-car (ref term))))
    (loop for head = (p2p-car (ref term)) then (p2p-car tail)
          and tail = (p2p-cdr (ref term)) then (p2p-cdr tail)
          collect (read-term (make-instance 'xsb-term :ref head))
          until (zerop (is-list tail)))))

(defun call-xsb-prolog (term)
  (let ((register-1-term (reg-term 1)))
    (p2p-unify register-1-term (ref term))
    (xsb-command)))

(defun open-xsb-query (term)
  (let ((register-1-term (reg-term 1)))
    (p2p-unify register-1-term (ref term))
    (xsb-query)))
