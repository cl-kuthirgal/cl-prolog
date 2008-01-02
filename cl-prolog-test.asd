
(in-package #:cl-prolog-system)

(defsystem cl-prolog-test
    :depends-on (:cl-prolog :cl-swi-client :fiveam)
    :components ((:module :cl-prolog-test
                          :serial t
                          :pathname "src/test/"
                          :components ((:file "package")
                                       (:file "cl-prolog-test")))))
