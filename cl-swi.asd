
(defsystem cl-swi
    :name "Common Lisp Prolog embedded SWI-Prolog backend"
    :author "Keith James"
    :version "0.3.0"
    :licence "GPL"
    :depends-on (:cl-prolog :cffi)
    :components ((:module :cl-swi
                          :pathname "src/swi/"
                          :components ((:file "package")
                                       (:file "specials"
                                              :depends-on ("package"))
                                       (:file "swi-prolog-cffi"
                                              :depends-on ("package"))
                                       (:file "classes"
                                              :depends-on ("package"))
                                       (:file "cl-swi"
                                              :depends-on ("package"
                                                           "specials"
                                                           "swi-prolog-cffi"
                                                           "classes"))))))