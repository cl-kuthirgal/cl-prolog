
(in-package :cl-user)

(defpackage #:cl-prolog-system
  (:use :common-lisp :asdf)
  (:export #:testsuite))


(in-package #:cl-prolog-system)

(defsystem cl-prolog
    :name "cl-prolog"
    :author "Keith James"
    :version "0.3.0"
    :licence "GPL"
    :depends-on (:cl-ppcre)
    :components ((:module :cl-prolog
                          :serial t
                          :pathname "src/"
                          :components ((:file "package")
                                       (:file "cl-prolog"))
                          :depends-on (:cl-prolog-sys))
                 (:module :cl-prolog-sys
                          :serial t
                          :pathname "src/"
                          :components ((:file "package")
                                       (:file "classes")
                                       (:file "generics")
                                       (:file "conditions")
                                       (:file "cl-prolog-sys")))))

(in-package #:asdf)

(defmethod perform ((op test-op) (c (eql (find-system
                                          'cl-prolog))))
  (operate 'load-op :cl-prolog-test)
  (funcall (intern (string :run!) (string :fiveam))
           'cl-prolog-system:testsuite))

(defmethod operation-done-p ((op test-op) (c (eql (find-system
                                                   'cl-prolog))))
  nil)
