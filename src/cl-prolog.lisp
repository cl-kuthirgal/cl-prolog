
(in-package :cl-prolog)

(defvar *current-prolog* nil)
(defvar *current-module* nil)

(defmacro defpredicate (symbol functor/arity
                        &optional (docstring functor/arity))
  (let* ((name (pl-functor functor/arity))
         (arity (pl-arity functor/arity))
         (args (loop for i from 1 to arity collect
                     (gensym (format nil "arg~a-" i)))))
    `(progn
      (defun ,symbol (,@args)
        ,docstring
        (create-compound-term ,name (list ,@args) *current-prolog*)))))

;; (defmacro defpredicate (symbol functor &optional (docstring functor))
;;   (let ((name (pl-functor functor)))
;;     `(progn
;;       (defun ,symbol (&rest args)
;;         ,docstring
;;         (create-compound-term ,name args *current-prolog*)))))

(defmacro with-prolog (prolog &body body)
  "Perform BODY with Prolog PROLOG."
  `(progv '(*current-prolog*)
    (list ,prolog)
    ,@body))

(defmacro with-module (module &body body)
  "Perform BODY with Prolog module MODULE."
   `(progv '(*current-module*)
    (list ,module)
    ,@body))

(defpredicate consult/1 "consult/1")
(defpredicate assert/1 "assert/1")
(defpredicate retractall/1 "retractall/1")

(defpredicate =/2 "=/2")
(defpredicate ==/2 "==/2")
(defpredicate not/1 "\+/1")
(defpredicate not==/2 "\\==/2")

(defpredicate is/2 "is/2")

(defpredicate member/2 "member/2")
(defpredicate findall/3 "findall/3")
(defpredicate bagof/3 "bagof/3")
(defpredicate setof/3 "setof/3")

(defpredicate listing/0 "listing/0")
(defpredicate listing/1 "listing/1")


(defun and/n (args)
  (reduce #'(lambda (&rest terms)
              (create-compound-term "," terms *current-prolog*))
          args :from-end t))

(defun or/n (args)
  (reduce #'(lambda (&rest terms)
              (create-compound-term ";" terms *current-prolog*))
          args :from-end t))

(defun start-prolog (prolog-type &rest args)
  (create-prolog args prolog-type))

(defun stop-prolog (&optional (prolog *current-prolog*))
  (destroy-prolog prolog))

(defun find-module (name)
  (find-prolog-module name *current-prolog*))

(defun call (expr)
  "Calls Prolog with EXPR in the context of *CURRENT-MODULE* using
*CURRENT-PROLOG*. Returns T if the call was successful, otherwise
NIL."
  (call-prolog expr *current-module* *current-prolog*))

(defun rule (head &rest body)
  (create-rule head body *current-prolog*))

(defun <- (expr)
  "Asserts EXPR in the context of *CURRENT-MODULE* using
*CURRENT-PROLOG*. Returns T if the call was successful, otherwise
NIL."
  (call-prolog (cons 'assert/1 (list expr)) *current-module*
               *current-prolog*))

(defun query (expr)
  "Queries EXPR in the context of *CURRENT-MODULE* using
*CURRENT-PROLOG*, returning two values. The first value is T if the
query produced a solution, otherwise NIL. The second value is a list
of any Prolog variable bindings. Only the first solution is returned."
  (prolog-query expr *current-module* *current-prolog*))

(defun open-query (expr)
  "Opens a query with EXPR in the context of *CURRENT-MODULE* using
*CURRENT-PROLOG*"
  (open-prolog-query expr *current-module* *current-prolog*))

(defun next-solution (query)
  "Gets the next solution for QUERY, returning two values. The first
value is T if the query produced a solution, otherwise NIL. The second
value is a list of any Prolog variable bindings."
  (prolog-next-solution query))

(defun close-query (query)
  "Closes QUERY."
  (close-prolog-query query))
