
;; (in-package #:cl-prolog-system)

(defsystem cl-swi-client
    :name "cl-swi-client"
    :author "Keith James"
    :version "0.3.0"
    :licence "GPL"
    :depends-on (:cl-prolog :usocket)
    :components ((:module :cl-swi-client
                          :serial t
                          :pathname "src/swi-client/"
                          :components ((:file "package")
                                       (:file "classes")
                                       (:file "cl-swi-client")))))
