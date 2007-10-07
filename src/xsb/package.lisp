
(defpackage #:xsb-prolog
  (:use #:common-lisp)
  (:documentation "XSB Prolog low-level interface.")
  (:export
   ;; typedefs
   #:prolog-int #:prolog-float #:reg-num #:prolog-term #:xsbbool #:xsbbyte

   #:ptoc-int
   #:ptoc-float
   #:ptoc-string
   #:ptoc-term

   #:ctop-int
   #:ctop-float
   #:ctop-string

   #:reg-term

   #:c2p-int
   #:c2p-float
   #:c2p-string
   #:c2p-list
   #:c2p-nil
   #:c2p-functor
   #:c2p-chars
   #:c2p-term

   #:p2c-int
   #:p2c-float
   #:p2c-string
   #:p2c-functor
   #:p2c-arity

   #:p2p-arg
   #:p2p-car
   #:p2p-cdr
   #:p2p-new
   #:p2p-call
   #:p2p-unify
   #:p2p-deref

   #:is-atom
   #:is-var
   #:is-int
   #:is-float
   #:is-string
   #:is-list
   #:is-nil
   #:is-functor
   #:is-charlist
   #:is-attv

   #:xsb-init
   #:xsb-init-string
   #:xsb-command
   #:xsb-command-string
   #:xsb-query
   #:xsb-next
   #:xsb-close-query
   #:xsb-close))

(defpackage #:cl-xsb
  (:use #:common-lisp #:cl-prolog #:cl-prolog-sys #:xsb-prolog)
  (:nicknames "xsb")
  (:documentation "XSB Prolog implementation."))