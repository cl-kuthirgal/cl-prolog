
(in-package :cl-prolog-sys)

(define-condition prolog-error (error)
  ((text :initarg :text :initform nil
         :reader text))
  (:report (lambda (condition stream)
             (format stream "A Prolog error occurred~@[: ~a~]."
                     (text condition)))))

(define-condition prolog-comm-error (prolog-error)
  ()
  (:report (lambda (condition stream)
             (format stream "A Prolog communication error occurred~@[: ~a~]."
                     (text condition)))))

(define-condition prolog-server-error (prolog-error)
  ()
  (:report (lambda (condition stream)
             (format stream "A Prolog server error occurred~@[: ~a~]."
                     (text condition)))))

(define-condition prolog-client-error (prolog-error)
  ()
  (:report (lambda (condition stream)
             (format stream "A Prolog client error occurred~@[: ~a~]."
                     (text condition)))))
