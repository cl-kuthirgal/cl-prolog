
(in-package #:cl-swi-client)

(defclass swi-prolog (prolog)
  ((server-hostname :initarg :server-hostname
                    :initform "localhost"
                    :accessor server-hostname-of
                    :documentation "The hostname of the SWI-Prolog
server. Defaults to localhost.")
   (server-port :initarg :server-port
                :initform 4321
                :accessor server-port-of
                :documentation "The port of the SWI-Prolog
server. Defaults to 4321.")
   (server-socket :initarg :server-socket
                  :initform nil
                  :accessor server-socket-of
                  :documentation "The current socket to the SWI-Prolog
server.")
   (module-cache :initarg :module-cache
                 :initform (make-hash-table :test #'equal)
                 :reader module-cache-of
                 :documentation "Map of names to modules.")
   (vars-terms :initarg :vars-terms
               :initform nil
               :accessor vars-terms-of
               :documentation "Map of Lisp symbols representing Prolog
variables to Prolog terms representing the same Prolog variables."))
  (:documentation ""))

(defclass swi-query (prolog-query)
  ((query-id :initform nil
             :initarg :query-id
             :reader query-id-of
             :documentation "The identifier of the query specified by
the Prolog server.")
   (pl-variables :initform nil
                 :initarg :pl-variables
                 :reader pl-variables-of
                 :documentation "A list of the Lisp Prolog variables
in the query."))
  (:documentation "An SWI-Prolog client query."))

(defclass swi-term (term name-mixin)
  ((pl-value :initarg :pl-value
             :accessor pl-value-of
             :documentation "The Prolog value of the term."))
  (:documentation "An SWI-Prolog term."))

(defclass swi-compound-term (swi-term compound-term)
  ()
  (:documentation "An SWI-Prolog compound term."))

(defclass swi-module (module name-mixin)
  ()
  (:documentation "An SWI-Prolog client module."))
