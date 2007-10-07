
(defsystem cl-swi-client
    :name "Common Lisp Prolog SWI-Prolog socket backend"
    :author "Keith James"
    :version "0.3.0"
    :licence "GPL"
    :depends-on (:cl-prolog :usocket)
    :components ((:module :cl-swi-client
                          :pathname "src/swi-client/"
                          :components ((:file "package")
                                       (:file "classes"
                                              :depends-on ("package"))
                                       (:file "cl-swi-client"
                                              :depends-on ("package"
                                                           "classes"))))))