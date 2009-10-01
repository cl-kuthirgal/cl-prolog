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

(in-package :cl-prolog-test)

(deftestsuite cl-prolog-tests ()
  ())

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

(when (or (null cl-prolog:*current-prolog*)
          (not (enabled-p cl-prolog:*current-prolog*)))
  (setf cl-prolog:*current-prolog*
        (start-prolog :swi-client-prolog "localhost" 4321))
  (setf cl-prolog:*current-module* (find-module "user")))

(addtest (cl-prolog-tests) pl-atom-p/1
  (is-true (pl-atom-p 'foo))
  (is-true (pl-atom-p '|fOO|))
  (is-true (pl-atom-p 'foo_bar))
  (is-false (pl-atom-p '|Foo|))
  (is-true (pl-atom-p '!)))

(addtest (cl-prolog-tests) pl-variable-p/1
  (is-true (pl-variable-p '?foo))
  (is-false (pl-variable-p 'foo)))

(addtest (cl-prolog-tests) anon-pl-variable-p/1
  (is-true (anon-pl-variable-p '?_))
  (is-false (anon-pl-variable-p '?foo)))

(addtest (cl-prolog-tests) pl-variable-name/1
  (is (string= "FOO" (pl-variable-name '?foo)))
  (is (string= "_" (pl-variable-name '?_)))
  (is (string= "FOO_BAR" (pl-variable-name '?foo-bar))))
  ;; (assert-error 'prolog-error (pl-variable-name 'foo)))

(addtest (cl-prolog-tests) pl-atom-name/1
  (is (string= "foo" (pl-atom-name 'foo)))
  (is (string= "fOO" (pl-atom-name '|fOO|))))
  ;; (assert-error 'prolog-error (pl-atom-name '|Foo|)))

(addtest (cl-prolog-tests) pl-quote-atom-p/1
  (is-false (cl-prolog-sys::pl-quote-atom-p "abc"))
  (is-false (cl-prolog-sys::pl-quote-atom-p "aBC"))
  (is-true (cl-prolog-sys::pl-quote-atom-p "Abc"))
  (is-false (cl-prolog-sys::pl-quote-atom-p "<="))
  (is-true (cl-prolog-sys::pl-quote-atom-p "a=")))

(addtest (cl-prolog-tests) pl-functor/arity/1
  (is (string= "foo" (cl-prolog-sys::pl-functor "foo/2")))
  (is (= 2 (cl-prolog-sys::pl-arity "foo/2"))))

(addtest (cl-prolog-tests) name-mangling/1
  (is (string= "foo-bar" (cl-prolog-sys::pl-name-to-lisp "foo_bar"))
  (is (string= "foo_bar" (cl-prolog-sys::lisp-name-to-pl "foo-bar")))))


(addtest (cl-prolog-tests) convert-atom/1
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x foo))
    (is-true success)
    (is (eql '|foo| (cdr (assoc '?x bindings))))))

(addtest (cl-prolog-tests) convert-integer/1
  (multiple-value-bind (success bindings)
      (query '(is/2 ?x 1))
    (is-true success)
    (is (= 1 (cdr (assoc '?x bindings))))))

(addtest (cl-prolog-tests) convert-float/1
  (multiple-value-bind (success bindings)
      (query '(is/2 ?x 1.1))
    (is-true success)
    (is (= 1.1 (cdr (assoc '?x bindings))))))

(addtest (cl-prolog-tests) convert-string/1
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x "foo"))
    (is-true success)
    (is (eql '|foo| (cdr (assoc '?x bindings))))))

(addtest (cl-prolog-tests) convert-list-of-atoms/1
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (foo bar)))
    (is-true success)
    (is (equal '(|foo| |bar|)  (cdr (assoc '?x bindings))))))

(addtest (cl-prolog-tests) convert-list-of-integers/1
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (1 2)))
    (is-true success)
    (is (equal '(1 2) (cdr (assoc '?x  bindings))))))

(addtest (cl-prolog-tests) convert-list-of-floats/1
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (1.1 2.2)))
    (is-true success)
    (is (equal '(1.1 2.2) (cdr (assoc '?x  bindings))))))

(addtest (cl-prolog-tests) convert-list-of-strings/1
  (multiple-value-bind (success bindings)
      (query '(=/2 ?x (|foo| |bar|)))
    (is-true success)
    (is (equal '(|foo| |bar|) (cdr (assoc '?x  bindings))))))

(addtest (cl-prolog-tests) convert-list-head-tail/1
  (multiple-value-bind (success bindings)
      (query '(=/2 (?h . ?t) (foo bar baz)))
    (is-true success)
    (is (eql '|foo| (cdr (assoc '?h bindings))))
    (is (equal '(|bar| |baz|) (cdr (assoc '?t  bindings))))))

(addtest (cl-prolog-tests) convert-list-head-tail/2
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

(addtest (cl-prolog-tests) zebra/1
  (setup-zebra)
  (multiple-value-bind (success bindings)
      (query '(zebra/3 ?houses ?water-drinker ?zebra-owner))
    (is-true success)
    (is (eql '|norwegian| (cdr (assoc '?water-drinker bindings))))
    (is (eql '|japanese| (cdr (assoc '?zebra-owner bindings))))))

;; (<- '(cl-prolog-test/1 (test-p/1 a)))
;; (<- '(cl-prolog-test/1 (test-p/1 b)))
;; (<- '(cl-prolog-test/1 (test-p/1 c)))
