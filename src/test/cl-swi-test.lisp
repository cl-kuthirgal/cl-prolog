
(in-package :cl-swi-test)

(defunctor retractall/1 "retractall/1")

(defunctor cl-prolog-test/1 "cl_prolog_test/1"
  "Predicate marking all cl-prolog test facts.")

(defunctor test-p/1 "test_p/1"
  "Test predicate.")

(call '(retractall/1 (cl-prolog-test/1 ?_)))

(<- '(cl-prolog-test/1 (test-p/1 a)))
(<- '(cl-prolog-test/1 (test-p/1 b)))
(<- '(cl-prolog-test/1 (test-p/1 c)))

(define-test convert-atom
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x foo))
    (assert-true success)
    (test-binding '?x "foo" bindings #'string=)))

(define-test convert-integer
  (multiple-value-bind (success bindings)
      (query '(is/2 ?x 1))
    (assert-true success)
    (test-binding '?x 1 bindings #'eq)))

(define-test convert-float
  (multiple-value-bind (success bindings)
      (query '(is/2 ?x (float 1.1d0)))
    (assert-true success)
    (test-binding '?x (float 1.1d0) bindings)))

(define-test convert-string
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x "foo"))
    (assert-true success)
    (test-binding '?x "foo" bindings #'string=)))

(define-test convert-list-of-atoms
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (foo bar)))
    (assert-true success)
    (test-binding '?x '("foo" "bar") bindings #'equal)))

(define-test convert-list-of-integers
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (1 2)))
    (assert-true success)
    (test-binding '?x '(1 2) bindings #'equal)))

(define-test convert-list-of-floats
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x ((float 1.1d0) (float 2.2d0))))
    (assert-true success)
    (test-binding '?x (list (float 1.1d0) (float 2.2d0)) bindings #'equal)))

(define-test convert-list-of-strings
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x ("foo" "bar")))
    (assert-true success)
    (test-binding '?x '("foo" "bar") bindings #'equal)))

(define-test convert-list-head-tail
  (multiple-value-bind (success bindings)
      (query '(=/2 (?h . ?t) (foo bar baz)))
    (assert-true success)
    (test-binding '?h "foo" bindings #'string=)
    (test-binding '?t '("bar" "baz") bindings #'equal)))

(define-test convert-list-head-tail/2
  (multiple-value-bind (success bindings)
      (query '(=/2 (?h1 ?h2 . ?t) (foo bar baz)))
    (assert-true success)
    (test-binding '?h1 "foo" bindings #'string=)
    (test-binding '?h2 "bar" bindings #'string=)
    (test-binding '?t '("baz") bindings #'equal)))

;; (define-test cut-test
;;   (let ((query (open-query '(cl-prolog-test/1 (test-p/1 ?x)))))
;;     (multiple-value-bind (success bindings)
;;         (next-solution query)
;;       (assert-true success)
;;       (test-binding '?x '\a bindings))
;;     (assert-false (next-solution query))
;;     (close-query query)))