
(in-package :cl-prolog-test-support)

(defmacro test-binding (variable value bindings &optional (test #'eql))
  "Tests that the Lisp symbol VARIABLE representing a Prolog variable
is associated with VALUE in the assoc list BINDINGS, using equality
TEST, which defaults to EQL."
  `(progn
    (assert-true (assoc ,variable ,bindings))
    (assert-equality ,test ,value (cdr (assoc ,variable ,bindings)))))