
(defpackage #:cl-prolog-sys
  (:use #:common-lisp)
  (:documentation "Prolog system interface.")
  (:export
   ;; Functions
   #:pl-atom-p
   #:pl-variable-p
   #:anon-pl-variable-p
   #:pl-atom-name
   #:pl-variable-name
   #:pl-functor-name
   #:pl-functor-arity
   #:pl-quote-atom-p
   #:pl-name-to-lisp
   #:lisp-name-to-pl
   #:proper-list-p
   #:dotted-pair-p
   #:dotted-list-head
   #:dotted-list-tail
   #:pl-expr-p
   #:apply-expression
   ;; Classes
   #:prolog
   #:prolog-query
   #:reference-mixin
   #:name-mixin
   #:term
   #:compound-term
   #:functor
   #:predicate
   #:module
   ;; Slots
   #:functor
   #:name
   #:arity
   #:ref
   #:module
   ;; Generic functions
   #:name-of
   #:ref-of
   #:functor-of
   #:arity-of
   #:args-of
   #:module-of
   #:prolog-of
   #:prolog-type
   #:prolog-state
   #:create-prolog
   #:destroy-prolog
   #:is-enabled-p
   #:create-compound-term
   #:create-rule
   #:find-prolog-module
   #:call-prolog
   #:prolog-query
   #:open-prolog-query
   #:prolog-next-solution
   #:cut-prolog-query
   #:close-prolog-query
   ;; Conditions
   #:prolog-error))

(defpackage #:cl-prolog
  (:use #:common-lisp #:cl-prolog-sys)
  (:nicknames "pl")
  (:documentation "Prolog user interface.")
  (:export
   ;; Parameters
   #:*current-prolog*
   #:*current-module*
   ;; Macros
   #:defunctor
   #:defpredicate
   #:with-prolog
   #:with-module
   ;; Interface
   #:start-prolog
   #:stop-prolog
   #:is-enabled-p
   #:find-module
   #:<-
   #:rule
   #:call
   #:query
   #:open-query
   #:next-solution
   #:cut-query
   #:close-query
   ;; Functors
   #:and/2
   #:and/n
   #:or/2
   #:or/n
   #:consult/1
   #:=/2
   #:==/2
   #:not==/2
   #:is/2
   #:findall/3
   #:bagof/3
   #:setof/3
   ;; Functions
   #:pl-atom-p
   #:pl-variable-p
   #:anon-pl-variable-p
   #:pl-atom-name
   #:pl-variable-name
   ;; Conditions
   #:prolog-error
   #:prolog-comm-error
   #:prolog-server-error
   #:prolog-client-error))
