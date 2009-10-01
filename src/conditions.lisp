;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
;;;
;;; This file is part of cl-prolog.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

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
