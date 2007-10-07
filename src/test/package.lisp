
(defpackage #:cl-prolog-test-support
  (:use #:common-lisp #:lisp-unit)
  (:documentation "Support utilites for tests.")
  (:export
   #:test-binding))

(defpackage #:cl-prolog-sys-test
  (:use #:common-lisp #:cl-prolog #:lisp-unit)
  (:documentation "cl-prolog-sys tests."))

(defpackage #:cl-swi-test
  (:use #:common-lisp #:cl-prolog #:cl-swi #:lisp-unit
        #:cl-prolog-test-support)
  (:documentation "cl-swi tests."))

(defpackage #:cl-swi-client-test
  (:use #:common-lisp #:cl-prolog #:cl-swi-client #:lisp-unit
        #:cl-prolog-test-support)
  (:documentation "cl-swi-client tests."))
