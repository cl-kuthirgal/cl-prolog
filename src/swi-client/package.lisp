
(defpackage #:cl-swi-client
  (:use #:common-lisp #:cl-prolog #:cl-prolog-sys #:usocket)
  (:nicknames "swic")
  (:documentation "SWI-Prolog client implementation.")
  (:export
   ;; accessors
   #:server-hostname-of
   #:server-port-of))