
(defsystem cl-swi-client-test
    :depends-on (:cl-prolog :cl-swi :cl-swi-client :lisp-unit)
    :components ((:module :cl-prolog-test
                          :pathname "src/test/"
                          :components ((:file "package")
                                       (:file "test-support")
                                       (:file "cl-prolog-sys-test"
                                              :depends-on ("package"))
                                       (:file "cl-swi-client-test"
                                              :depends-on ("package"
                                                           "test-support"))
                                       )))
    :in-order-to ((test-op (load-op cl-swi-client-test))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'lisp-unit)
    (asdf:oos 'asdf:load-op 'lisp-unit))
  (unless (find-package 'cl-prolog)
    (asdf:oos 'asdf:load-op 'cl-prolog)))

(defmethod perform ((operation test-op)
                    (component (eql (find-system 'cl-swi-client-test))))
  (let* ((cl-prolog:*current-prolog* (cl-prolog:start-prolog
                                      :swi-client-prolog "localhost" 4321))
         (cl-prolog:*current-module* (cl-prolog:find-module "user")))
    (prog1
        (values
         (lisp-unit:run-all-tests :cl-prolog-sys-test)
         (lisp-unit:run-all-tests :cl-swi-client-test))
      (cl-prolog:stop-prolog))))

(defmethod operation-done-p ((operation test-op)
                             (component (eql (find-system 'cl-swi-client-test))))
  (values nil))
