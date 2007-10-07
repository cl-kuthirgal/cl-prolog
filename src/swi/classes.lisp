
(in-package #:cl-swi)

(defclass swi-prolog (prolog)
  ((argv-ptr :initarg :argv-ptr
             :accessor argv-ptr-of
             :documentation "Pointer to the Prolog init args which
must remain available until the instance is destroyed, when the
pointer is freed.")
   (module-cache :initarg :module-cache
                 :initform (make-hash-table :test #'equal)
                 :reader module-cache-of
                 :documentation "Map of names to modules.")
   (functor-cache :initarg :functor-cache
                  :initform (make-hash-table :test #'equal)
                  :reader functor-cache-of
                  :documentation "Map of name/arity tuples to
functors.")
   (predicate-cache :initarg :predicate-cache
                    :initform (make-hash-table :test #'equal)
                    :reader predicate-cache-of
                    :documentation "Map of name/arity/module tuples to
predicates.")
   (vars-terms :initarg :vars-terms
               :initform nil
               :accessor vars-terms-of
               :documentation "Map of Lisp symbols representing Prolog
variables to Prolog terms representing the same Prolog variables.")
   (terms-vars :initarg :terms-vars
               :initform nil
               :accessor terms-vars-of
               :documentation "Map of Prolog terms representing Prolog
variables to Lisp symbols representing the same Prolog variables."))
  (:documentation "The SWI-Prolog backend."))

(defclass swi-query (prolog-query reference-mixin)
  ((frame :initform nil
          :initarg :frame
          :reader frame-of
          :documentation "Current SWI-Prolog foreign frame (see SWI
Prolog foreign interface documentation.")
   (is-open :initform t
            :accessor is-open
            :documentation "Flag indicating whether the query is
currently open and can offer further solutions or can be closed."))
  (:documentation "An SWI-Prolog query."))

(defclass swi-term (term reference-mixin)
  ()
  (:documentation "An SWI-Prolog term."))

(defclass swi-compound-term (swi-term compound-term)
  ()
  (:documentation "An SWI-Prolog compound term."))

(defclass swi-module (module reference-mixin)
  ()
  (:documentation "An SWI-Prolog module."))

(defclass swi-functor (functor reference-mixin)
  ()
  (:documentation "An SWI-Prolog functor."))

(defclass swi-predicate (predicate reference-mixin)
  ()
  (:documentation "An SWI-Prolog predicate."))
