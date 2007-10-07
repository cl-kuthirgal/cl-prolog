
(defsystem cl-prolog
    :name "Common Lisp Prolog"
    :author "Keith James"
    :version "0.3.0"
    :licence "GPL"
    :depends-on (:cl-ppcre)
    :components ((:module :cl-prolog
                          :pathname "src/"
                          :components ((:file "package")
                                       (:file "specials"
                                              :depends-on ("package"))
                                       (:file "cl-prolog"
                                              :depends-on ("package"
                                                           "specials")))
                          :depends-on (:cl-prolog-sys))
                 (:module :cl-prolog-sys
                          :pathname "src/"
                          :components ((:file "package")
                                       (:file "classes"
                                              :depends-on ("package"))
                                       (:file "generics"
                                              :depends-on ("package"))
                                       (:file "conditions"
                                              :depends-on ("package"))
                                       (:file "cl-prolog-sys"
                                              :depends-on ("package"
                                                           "classes"
                                                           "generics"
                                                           "conditions"))))))

