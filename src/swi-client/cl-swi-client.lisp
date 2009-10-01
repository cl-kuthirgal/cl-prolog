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

(in-package :cl-swi-client)

(defmethod print-object ((obj swi-term) stream)
  (with-slots (pl-value) obj
    (format stream "<SWI-TERM pl-value: ~a>"
            pl-value)))

(defmethod print-object ((obj swi-module) stream)
  (with-slots (name) obj
    (format stream "<SWI-MODULE name: ~a>"
            name)))

(defmethod write-term ((obj swi-term) (arg symbol))
  (if (pl-variable-p arg)
      (ensure-variable arg obj)
     (let ((pl-name (format nil "'~a'" (pl-atom-name arg))))
       (setf (name-of obj) pl-name)
       (setf (pl-value-of obj) pl-name)
       pl-name))
  obj)

(defmethod write-term ((obj swi-term) (arg integer))
  (setf (pl-value-of obj) arg)
  obj)

(defmethod write-term ((obj swi-term) (arg float))
  (setf (pl-value-of obj) arg)
  obj)

(defmethod write-term ((obj swi-term) (arg string))
  ;; Flag backquoted_string is set on server to let us distinguish
  ;; strings from atoms using SWI-Prolog's backquote syntax
  ;; (setf (pl-value obj) (format nil "`~a`" arg))
  (setf (pl-value-of obj) (format nil "'~a'" arg))
  obj)

(defmethod write-term ((obj swi-term) (arg swi-term))
  (setf (pl-value-of obj) (pl-value-of arg))
  obj)

(defmethod write-term ((obj swi-term) (arg (eql nil)))
  (setf (pl-value-of obj) "[]")
  obj)

(defmethod write-term ((obj swi-term) (arg cons))
  (let ((prolog (prolog-of obj)))
    (cond ((dotted-pair-p arg)
           (let ((head (make-instance 'swi-term :prolog prolog))
                 (tail (make-instance 'swi-term :prolog prolog)))
             (write-term head (car arg))
             (write-term tail (cdr arg))
             (setf (pl-value-of obj)
                   (format nil "[~a|~a]" (pl-value-of head)
                           (pl-value-of tail)))))
          ((proper-list-p arg)
           (let ((terms (mapcar #'write-term (create-term-list
                                              (length arg) prolog) arg)))
           (setf (pl-value-of obj)
                 (format nil "[~{~a~^, ~}]"
                         (mapcar #'pl-value-of terms)))))
        (t
         (let* ((head (dotted-list-head arg))
                (tail (dotted-list-tail arg))
                (head-terms
                 (mapcar #'write-term
                         (create-term-list (length head) prolog) head))
                (tail-term
                 (write-term (make-instance 'swi-term
                                            :prolog prolog) tail)))
           (setf (pl-value-of obj)
                 (format nil "[~{~a~^, ~}|~a]"
                         (mapcar #'pl-value-of head-terms)
                         (pl-value-of tail-term)))))))
  obj)

(defun terminate-pl-clause (clause-string)
  "Returns a copy of CLAUSE-STRING terminated with a full stop and
newline."
  (format nil "~a.~%" clause-string))

(defun create-term-list (arity prolog)
  "Creates a list of length ARITY containing terms."
  (if (zerop arity)
      nil
    (cons (make-instance 'swi-term :prolog prolog)
          (create-term-list (1- arity) prolog))))

(defun ensure-module (name prolog)
  "Returns an SWI-Prolog client module of NAME."
   (multiple-value-bind (module cached)
       (gethash name (module-cache-of prolog))
     (if cached
         module
       (setf (gethash name (module-cache-of prolog))
             (make-instance 'swi-module :name name)))))

(defun ensure-variable (symbol term)
  "Associates SYMBOL with a free Prolog variable TERM and returns
TERM. If SYMBOL is already associated with a Prolog variable, the
pl-value slot of the cached value is used to set TERM's pl-value
slot."
  (let ((prolog (prolog-of term)))
    (if (anon-pl-variable-p symbol)
        (setf (pl-value-of term) (pl-variable-name symbol))
      (multiple-value-bind (value cached)
          (gethash symbol (vars-terms-of prolog))
        (if cached
            (setf (pl-value-of term) (pl-value-of value))
          (progn
            (setf (pl-value-of term) (pl-variable-name symbol)
                  (gethash symbol (vars-terms-of prolog)) term))))))
  term)

(defun prolog-variable-bindings (solution prolog)
  "Returns the current variable bindings for the Prolog sexp SOLUTION
for PROLOG."
  (let ((terms (if (atom solution)
                   (list solution)
                 solution))
        (variables (loop for var being the hash-keys of
                        (vars-terms-of prolog)
                      collect var))
        (bindings nil))
    (mapcar #'(lambda (term var)
                (push (cons var term) bindings))
            terms variables)
    (nreverse bindings)))

(defun send-prolog-message (stream message)
  (format t "Sending ~a" message)
  (format stream "~a~%" message)
  (force-output stream))

(defun receive-prolog-reply (stream)
  (let* ((str (read-from-string (read-line stream)))
         (reply (eval str)))
    (when (and (listp reply)
               (stringp (first reply))
               (string-equal "CLIENT_INPUT_ERROR" (first reply)))
      (error 'prolog-client-error :text (format nil "~a" reply)))
    (when (and (listp reply)
               (stringp (first reply))
               (string-equal "SERVER_ERROR" (first reply)))
      (error 'prolog-server-error :text (format nil "~a" reply)))
    reply))

(defmethod cl-prolog-sys:create-prolog ((args list)
                          (prolog-type (eql :swi-client-prolog)))
  (destructuring-bind (hostname port)
      args
    (make-instance 'swi-prolog
                   :server-hostname hostname
                   :server-port port
                   :server-socket (socket-connect hostname port))))

;; FIXME -- send client_finished before closing the stream
(defmethod cl-prolog-sys:destroy-prolog ((prolog swi-prolog))
  (let ((server-stream (socket-stream (server-socket-of prolog))))
    (cond ((enabled-p prolog)
           (send-prolog-message "client_finished")
           (format t "On closing got ~a~%" (receive-prolog-reply stream))
           (close server-stream))
          ((and server-stream
                (not (open-stream-p server-stream)))
           (error 'prolog-comm-error "SWI-Prolog client connection closed"))
          (t
           (error 'prolog-comm-error "SWI-Prolog client not connected")))))

(defmethod cl-prolog-sys:enabled-p ((prolog swi-prolog))
  (let ((server-stream (socket-stream (server-socket-of prolog))))
    (and server-stream (open-stream-p server-stream))))

(defmethod cl-prolog-sys:create-compound-term ((name string) (args list)
                                               (prolog swi-prolog))
  (let ((terms (mapcar #'write-term
                       (create-term-list (length args) prolog) args)))
    (make-instance 'swi-compound-term :prolog prolog
                   :functor name :args args
                   :pl-value (format nil "'~a'(~{~a~^, ~})" name
                                     (mapcar #'pl-value-of terms)))))

(defmethod cl-prolog-sys:create-rule ((head swi-term) (body list)
                                      (prolog swi-prolog))
  (if (< (length body) 2)
      (create-compound-term ":-" (adjoin head body) prolog)
    (create-compound-term ":-" (list head (and/n body)) prolog)))


(defmethod cl-prolog-sys:find-prolog-module ((name string)
                                             (prolog swi-prolog))
  (ensure-module name prolog))

(defmethod cl-prolog-sys:call-prolog ((expr list) (module swi-module)
                                      (prolog swi-prolog))
  (let ((stream (socket-stream (server-socket-of prolog)))
        (clause (pl-value-of (apply-expression expr))))
    (send-prolog-message stream (terminate-pl-clause
                                 (format nil "client_call(~a)" clause)))
    (receive-prolog-reply stream)))

(defmethod cl-prolog-sys:call-prolog :around ((expr list)
                                              (module swi-module)
                                              (prolog swi-prolog))
  (declare (ignore expr module))
  (setf (vars-terms-of prolog) (make-hash-table))
  (call-next-method))

(defmethod cl-prolog-sys:prolog-query ((expr list) (module swi-module)
                                       (prolog swi-prolog))
  (let ((query (open-query expr)))
    (multiple-value-bind (solution bindings)
        (next-solution query)
      (close-query query)
      (values solution bindings))))

(defmethod cl-prolog-sys:open-prolog-query ((expr list) (module swi-module)
                                            (prolog swi-prolog))
  (let ((stream (socket-stream (server-socket-of prolog)))
        (pl-query (pl-value-of (apply-expression expr)))
        (vars-term (make-instance 'swi-term :prolog prolog)))
    (write-term vars-term
                (loop for var being the hash-keys of (vars-terms-of prolog)
                      collect var))
    (send-prolog-message stream
                         (terminate-pl-clause
                          (format nil "client_open_query(~a, ~a)"
                                  pl-query (pl-value-of vars-term))))
    (make-instance 'swi-query :prolog prolog)))
                   ;; :query-id (receive-prolog-reply stream))))

(defmethod cl-prolog-sys:open-prolog-query :around ((expr list)
                                             (module swi-module)
                                             (prolog swi-prolog))
  (declare (ignore expr module))
  (setf (vars-terms-of prolog) (make-hash-table))
  (call-next-method))

(defmethod cl-prolog-sys:prolog-next-solution ((query swi-query))
  (let* ((prolog (prolog-of query))
         (stream (socket-stream (server-socket-of prolog)))
         (solution nil))
    (send-prolog-message stream (terminate-pl-clause
                                 (format nil "client_next_solution")))
    (setf solution (receive-prolog-reply stream))
    ;; FIXME -- do correct thing when "solution" is no_more_solutions
    (values (not (null solution))
            (prolog-variable-bindings solution prolog))))

(defmethod cl-prolog-sys:close-prolog-query ((query swi-query))
  (let ((stream (socket-stream (server-socket-of (prolog-of query)))))
    (send-prolog-message stream (terminate-pl-clause "client_close_query"))
    (receive-prolog-reply stream)))
