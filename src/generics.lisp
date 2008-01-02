
(in-package :cl-prolog-sys)

(defgeneric read-term (term)
  (:documentation "Reads Lisp data from TERM."))

(defgeneric write-term (term)
  (:documentation ""))

(defgeneric create-compound-term (name args prolog)
  (:documentation "Creates a compound Prolog term for functor NAME
with ARGS using the PROLOG backend."))

(defgeneric find-prolog-module (name prolog)
  (:documentation "Retrieves a representation of the module NAME using
the PROLOG backend."))

(defgeneric create-prolog (args prolog-type)
  (:documentation "Creates the PROLOG-TYPE backend with arguments
ARGS. Returns a prolog instance if successful, or NIL otherwise."))

(defgeneric destroy-prolog (prolog)
  (:documentation "Removes the PROLOG backend."))

(defgeneric enabled-p (prolog)
  (:documentation "Returns T if the PROLOG backend is enabled, or NIL
otherwise."))

(defgeneric call-prolog (expr module prolog)
  (:documentation "Calls EXPR in the context of MODULE using the
PROLOG backend, returning T on success, or NIL otherwise."))

(defgeneric create-rule (head body prolog)
  (:documentation "Creates a Prolog rule for HEAD given BODY using the
PROLOG backend."))

(defgeneric prolog-query (expr module prolog)
  (:documentation "Queries EXPR in the context of MODULE using the
PROLOG backend."))

(defgeneric open-prolog-query (expr module prolog)
  (:documentation "Starts a qeuery EXPR in the context of
MODULE using the PROLOG backend."))

(defgeneric prolog-next-solution (query)
  (:documentation "Gets the next solution for QUERY."))

(defgeneric cut-prolog-query (query)
  (:documentation "Cuts the QUERY, discarding further solutions."))

(defgeneric close-prolog-query (query)
  (:documentation "Closes the QUERY."))
