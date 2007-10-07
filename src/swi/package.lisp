
(defpackage #:swi-prolog-cffi
  (:use #:common-lisp #:cffi)
  (:documentation "SWI-Prolog foreign library interface.")
  (:export
   ;; typedefs
   #:atom-t #:module-t #:predicate-t #:term-t #:qid-t #:pl-fid-t #:functor-t
   #:foreign-t
   ;; module functions
   #:pl-new-module
   ;; frame functions
   #:pl-open-foreign-frame
   #:pl-close-foreign-frame
   ;; foreign functions
   #:pl-register-foreign
   ;; predicate functions
   #:pl-pred
   #:pl-predicate
   ;; prolog interaction functions
   #:pl-open-query
   #:pl-next-solution
   #:pl-close-query
   #:pl-cut-query
   #:pl-call
   #:pl-call-predicate
   ;; exception functions
   #:pl-exception
   #:pl-raise-exception
   #:pl-throw
   ;; term reference functions
   #:pl-new-term-refs
   #:pl-new-term-ref
   #:pl-copy-term-ref
   ;; atom functions
   #:pl-new-atom
   #:pl-atom-chars
   ;; functor functions
   #:pl-new-functor
   ;; functions for reading from terms
   #:pl-get-atom
   #:pl-get-bool
   #:pl-get-atom-chars
   #:pl-get-string
   #:pl-get-chars
   #:pl-get-integer
   #:pl-get-long
   #:pl-get-float
   #:pl-get-functor
   #:pl-get-name-arity
   #:pl-get-module
   #:pl-get-arg
   #:pl-get-list
   #:pl-get-head
   #:pl-get-tail
   #:pl-get-nil
   ;; term type functions
   #:pl-term-type
   #:pl-is-variable
   #:pl-is-ground
   #:pl-is-atom
   #:pl-is-integer
   #:pl-is-string
   #:pl-is-float
   #:pl-is-compound
   #:pl-is-functor
   #:pl-is-list
   #:pl-is-atomic
   #:pl-is-number
   ;; term assignment functions
   #:pl-put-variable
   #:pl-put-atom
   #:pl-put-atom-chars
   #:pl-put-string-chars
   #:pl-put-integer
   #:pl-put-pointer
   #:pl-put-float
   #:pl-put-functor
   #:pl-put-list
   #:pl-put-nil
   #:pl-put-term
   ;; compound term functions
   #:pl-cons-functor-v
   #:pl-cons-list
   ;; unification functions

   ;; misc functions

   ;; startup/shutdown functions
   #:pl-initialise
   #:pl-is-initialised
   #:pl-cleanup
   #:pl-halt
   #:pl-query))

(defpackage #:cl-swi
  (:use #:common-lisp #:cl-prolog #:cl-prolog-sys #:swi-prolog-cffi)
  (:nicknames "swi")
  (:documentation "SWI-Prolog implementation."))
