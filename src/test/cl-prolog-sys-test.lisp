
(in-package :cl-prolog-sys-test)

(define-test pl-atom-p ()
  (assert-true (pl-atom-p 'foo))
  (assert-true (pl-atom-p '|fOO|))
  (assert-true (pl-atom-p 'foo_bar))
  (assert-false (pl-atom-p '|Foo|))
  (assert-true (pl-atom-p '!)))

(define-test pl-variable-p ()
  (assert-true (pl-variable-p '?foo))
  (assert-false (pl-variable-p 'foo)))

(define-test anon-pl-variable-p ()
  (assert-true (anon-pl-variable-p '?_))
  (assert-false (anon-pl-variable-p '?foo)))

(define-test pl-variable-name ()
  (assert-equality #'string= "FOO" (pl-variable-name '?foo))
  (assert-equality #'string= "_" (pl-variable-name '?_))
  (assert-error 'prolog-error (pl-variable-name 'foo)))

(define-test pl-atom-name ()
  (assert-equality #'string= "foo" (pl-atom-name 'foo))
  (assert-equality #'string= "fOO" (pl-atom-name '|fOO|)))
  ;; (assert-error 'prolog-error (pl-atom-name '|Foo|)))

(define-test pl-quote-atom-p ()
  (assert-false (cl-prolog-sys::pl-quote-atom-p "abc"))
  (assert-false (cl-prolog-sys::pl-quote-atom-p "aBC"))
  (assert-true (cl-prolog-sys::pl-quote-atom-p "Abc"))
  (assert-false (cl-prolog-sys::pl-quote-atom-p "<="))
  (assert-true (cl-prolog-sys::pl-quote-atom-p "a=")))

(define-test pl-functor-name/arity ()
  (assert-equality #'string= "foo"
                   (cl-prolog-sys::pl-functor-name "foo/2"))
  (assert-eq 2 (cl-prolog-sys::pl-functor-arity "foo/2")))

(define-test name-mangling ()
  (assert-equality #'string= "foo-bar"
                   (cl-prolog-sys::pl-name-to-lisp "foo_bar"))
  (assert-equality #'string= "foo_bar"
                   (cl-prolog-sys::lisp-name-to-pl "foo-bar")))
