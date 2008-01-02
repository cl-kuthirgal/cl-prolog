
(in-package :cl-prolog-sys)

(defclass reference-mixin ()
  ((ref :initarg :ref
        :accessor ref-of
        :documentation "A reference to the Prolog entity."))
  (:documentation "A mixin class providing a slot which contains a
reference or handle to a Prolog entity such as a term, functor or
module."))

(defclass name-mixin ()
  ((name :initform nil
         :initarg :name
         :accessor name-of
         :documentation "The name of the named object."))
  (:documentation "A mixin class providing a slot which contains a
name"))

(defclass prolog ()
  ()
  (:documentation "A Prolog backend."))

(defclass prolog-query ()
  ((prolog :initarg :prolog
           :reader prolog-of
           :documentation "The Prolog to which the query applies."))
  (:documentation "A Prolog query handle. Once a query handle is
obtained, possible answers may be obtained successively from it until
there are not further answers or the handle is relased."))

(defclass term ()
  ((prolog :initarg :prolog
           :reader prolog-of
           :documentation "The Prolog to which the term belongs."))
  (:documentation "A Prolog term."))

(defclass compound-term (term)
  ((functor :initform nil
            :initarg :functor
            :reader functor-of
            :documentation "The functor component of the compound
term.")
   (args :initform nil
         :initarg :args
         :reader args-of
         :documentation "The functor arguments of the compound term."))
  (:documentation "A Prolog compound term."))

(defclass module (name-mixin)
  ()
  (:documentation "A Prolog module."))

(defclass functor (name-mixin)
  ((arity :initarg :arity
          :accessor arity-of
          :documentation "The functor arity."))
  (:documentation "A Prolog functor."))

(defclass predicate ()
  ((functor :initform nil
            :initarg :functor
            :reader functor-of
            :documentation "The predicate functor.")
   (module :initform nil
           :initarg :module
           :reader module-of
           :documentation "The module namespace of the predicate."))
  (:documentation "A Prolog predicate."))
