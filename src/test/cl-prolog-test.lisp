
(in-package :cl-prolog-system)

(fiveam:def-suite testsuite
    :description "The test suite.")


(in-package :cl-prolog-test)

(defpredicate cl-prolog-test/1 "cl_prolog_test/1"
  "Predicate marking all cl-prolog test facts.")
(defpredicate cl-prolog-test/2 "cl_prolog_test/2"
  "Predicate marking cl-prolog cut tests.")
(defpredicate test-p/1 "test_p/1"
  "Test predicate.")

(defpredicate nextto/3 "nextto/3")
(defpredicate iright/3 "iright/3")
(defpredicate zebra/3 "zebra/3")
(defpredicate house/5 "house/5")


(in-suite cl-prolog-system:testsuite)

(when (or (null cl-prolog:*current-prolog*)
          (not (enabled-p cl-prolog:*current-prolog*)))
  (setf cl-prolog:*current-prolog*
        (start-prolog :swi-client-prolog "localhost" 4321))
  (setf cl-prolog:*current-module* (find-module "user")))

(test pl-atom-p
  (is-true (pl-atom-p 'foo))
  (is-true (pl-atom-p '|fOO|))
  (is-true (pl-atom-p 'foo_bar))
  (is-false (pl-atom-p '|Foo|))
  (is-true (pl-atom-p '!)))

(test pl-variable-p
  (is-true (pl-variable-p '?foo))
  (is-false (pl-variable-p 'foo)))

(test anon-pl-variable-p
  (is-true (anon-pl-variable-p '?_))
  (is-false (anon-pl-variable-p '?foo)))

(test pl-variable-name
  (is (string= "FOO" (pl-variable-name '?foo)))
  (is (string= "_" (pl-variable-name '?_)))
  (is (string= "FOO_BAR" (pl-variable-name '?foo-bar))))
  ;; (assert-error 'prolog-error (pl-variable-name 'foo)))

(test pl-atom-name
  (is (string= "foo" (pl-atom-name 'foo)))
  (is (string= "fOO" (pl-atom-name '|fOO|))))
  ;; (assert-error 'prolog-error (pl-atom-name '|Foo|)))

(test pl-quote-atom-p
  (is-false (cl-prolog-sys::pl-quote-atom-p "abc"))
  (is-false (cl-prolog-sys::pl-quote-atom-p "aBC"))
  (is-true (cl-prolog-sys::pl-quote-atom-p "Abc"))
  (is-false (cl-prolog-sys::pl-quote-atom-p "<="))
  (is-true (cl-prolog-sys::pl-quote-atom-p "a=")))

(test pl-functor/arity
  (is (string= "foo" (cl-prolog-sys::pl-functor "foo/2")))
  (is (= 2 (cl-prolog-sys::pl-arity "foo/2"))))

(test name-mangling
  (is (string= "foo-bar" (cl-prolog-sys::pl-name-to-lisp "foo_bar"))
  (is (string= "foo_bar" (cl-prolog-sys::lisp-name-to-pl "foo-bar")))))


(test convert-atom
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x foo))
    (is-true success)
    (is (eql '|foo| (cdr (assoc '?x bindings))))))

(test convert-integer
  (multiple-value-bind (success bindings)
      (query '(is/2 ?x 1))
    (is-true success)
    (is (= 1 (cdr (assoc '?x bindings))))))

(test convert-float
  (multiple-value-bind (success bindings)
      (query '(is/2 ?x 1.1))
    (is-true success)
    (is (= 1.1 (cdr (assoc '?x bindings))))))

(test convert-string
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x "foo"))
    (is-true success)
    (is (eql '|foo| (cdr (assoc '?x bindings))))))

(test convert-list-of-atoms
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (foo bar)))
    (is-true success)
    (is (equal '(|foo| |bar|)  (cdr (assoc '?x bindings))))))

(test convert-list-of-integers
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (1 2)))
    (is-true success)
    (is (equal '(1 2) (cdr (assoc '?x  bindings))))))

(test convert-list-of-floats
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (1.1 2.2)))
    (is-true success)
    (is (equal '(1.1 2.2) (cdr (assoc '?x  bindings))))))

(test convert-list-of-strings
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (|foo| |bar|)))
    (is-true success)
    (is (equal '(|foo| |bar|) (cdr (assoc '?x  bindings))))))

(test convert-list-head-tail
  (multiple-value-bind (success bindings)
      (query '(=/2 (?h . ?t) (foo bar baz)))
    (is-true success)
    (is (eql '|foo| (cdr (assoc '?h bindings))))
    (is (equal '(|bar| |baz|) (cdr (assoc '?t  bindings))))))

(test convert-list-head-tail/2
  (multiple-value-bind (success bindings)
      (query '(=/2 (?h1 ?h2 . ?t) (foo bar baz)))
    (is-true success)
    (is (eql '|foo| (cdr (assoc '?h1 bindings))))
    (is (eql '|bar| (cdr (assoc '?h2  bindings))))
    (is (equal '(|baz|) (cdr (assoc '?t  bindings))))))


(defun setup-zebra ()
  ;; (call '(retractall/1 (iright/3 ?_ ?_ ?_)))
  ;; (call '(retractall/1 (nextto/3 ?_ ?_ ?_)))
  ;; (call '(retractall/1 (zebra/3 ?_ ?_ ?_)))

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
        (member/2 (house/5 ?z zebra ?_ ?_ ?_) ?h))))

(test zebra
  (setup-zebra)
  (multiple-value-bind (success bindings)
      (query '(zebra/3 ?houses ?water-drinker ?zebra-owner))
    (is-true success)
    (is (eql '|norwegian| (cdr (assoc '?water-drinker bindings))))
    (is (eql '|japanese| (cdr (assoc '?zebra-owner bindings))))))

;; (<- '(cl-prolog-test/1 (test-p/1 a)))
;; (<- '(cl-prolog-test/1 (test-p/1 b)))
;; (<- '(cl-prolog-test/1 (test-p/1 c)))