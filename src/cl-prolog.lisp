
(in-package #:cl-prolog)

(defmacro defunctor (symbol functor &optional (docstring functor))
  (let* ((name (pl-functor-name functor))
         (arity (pl-functor-arity functor))
         (args (loop for i from 1 to arity collect
                     (gensym (format nil "arg~a-" i)))))
    `(progn
      (defun ,symbol (,@args)
        ,docstring
        (create-compound-term ,name (list ,@args) *current-prolog*)))))

(defmacro defpredicate (symbol functor)
  (let ((name (pl-functor-name functor)))
    `(progn
      (defun ,symbol (&rest args)
        (create-compound-term ,name args *current-prolog*)))))

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

(defunctor consult/1 "consult/1")

(defunctor assert/1 "assert/1")

(defunctor =/2 "=/2")
(defunctor ==/2 "==/2")
(defunctor not/1 "\+/1")
(defunctor not==/2 "\\==/2")

(defunctor is/2 "is/2")

(defunctor findall/3 "findall/3")
(defunctor bagof/3 "bagof/3")
(defunctor setof/3 "setof/3")


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

;;; (reduce #'(lambda (x y) (concatenate 'string x ", " y)) '("A" "B" "C"))


#|

(in-package :cl-prolog)

(defunctor use-module/1 "use_module/1")

(defunctor trace/0 "trace/0")
(defunctor guitracer/0 "guitracer/0")
(defunctor notrace/0 "notrace/0")

(defunctor member/2 "member/2")
(defunctor listing/1 "listing/1")

(defunctor nextto/3 "nextto/3")
(defunctor iright/3 "iright/3")
(defunctor zebra/3 "zebra/3")
(defunctor house/5 "house/5")

(setf *current-prolog* (start-prolog :swi-prolog
                                     "/usr/local/bin/pl" "-nosignals"))
(setf *current-module* (find-module "user"))

(<- '(iright/3 ?left ?right (?left ?right . ?_)))
(<- '(rule
      (iright/3 ?left ?right (?_ . ?rest))
      (iright/3 ?left ?right ?rest)))

(<- '(rule
      (nextto/3 ?x ?y ?list)
      (iright/3 ?x ?y ?list)))
(<- '(rule
      (nextto/3 ?x ?y ?list)
      (iright/3 ?y ?x ?list)))

(<- '(rule
      (zebra/3 ?h ?w ?z)
      (=/2 ?h ((house/5 norwegian ?_ ?_ ?_ ?_)
               ?_
               (house/5 ?_ ?_ ?_ milk ?_) ?_ ?_))
      (member/2 (house/5 englishman ?_ ?_ ?_ red) ?h)
      (member/2 (house/5 spaniard dog ?_ ?_ ?_) ?h)
      (member/2 (house/5 ?_ ?_ ?_ coffee green) ?h)
      (member/2 (house/5 ukrainian ?_ ?_ tea ?_) ?h)
      (iright/3 (house/5 ?_ ?_ ?_ ?_ ivory)
       (house/5 ?_ ?_ ?_ ?_ green) ?h)
      (member/2 (house/5 ?_ snails winston ?_ ?_) ?h)
      (member/2 (house/5 ?_ ?_ kools ?_ yellow) ?h)
      (nextto/3 (house/5 ?_ ?_ chesterfield ?_ ?_)
       (house/5 ?_ fox ?_ ?_ ?_) ?h)
      (nextto/3 (house/5 ?_ ?_ kools ?_ ?_)
       (house/5 ?_ horse ?_ ?_ ?_) ?h)
      (member/2 (house/5 ?_ ?_ luckystrike oj ?_) ?h)
      (member/2 (house/5 japanese ?_ parliaments ?_ ?_) ?h)
      (nextto/3 (house/5 norwegian ?_ ?_ ?_ ?_)
       (house/5 ?_ ?_ ?_ ?_ blue) ?h)
      (member/2 (house/5 ?w ?_ ?_ water ?_) ?h)
      (member/2 (house/5 ?z zebra ?_ ?_ ?_) ?h)))

(query '(zebra/3 ?houses ?water-drinker ?zebra-owner))

(defun test ()
  (dotimes (n 1000)
    (query '(zebra/3 ?_ ?water-drinker ?zebra-owner))))

|#

