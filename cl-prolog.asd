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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:find-system :deoxybyte-systems nil)
    (asdf:operate 'asdf:load-op :deoxybyte-systems)))

(defpackage :cl-prolog-system
  (:use :common-lisp :asdf)
  (:import-from :deoxybyte-systems :lift-test-config :cldoc-config))

(in-package :cl-prolog-system)

(defsystem cl-prolog
    :name "Common Lisp Prolog"
    :author "Keith James"
    :version "0.3.0"
    :licence "GPL v3"
    :depends-on ((:version :cl-ppcre "2.0.0"))
    :in-order-to ((test-op (load-op :cl-prolog :cl-prolog-test)))
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
                                       (:file "cl-prolog-sys")))
                 (:lift-test-config :lift-tests
                                    :pathname "cl-prolog-test.config"
                                    :target-system :cl-prolog)))
