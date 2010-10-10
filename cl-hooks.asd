;;; cl-hooks.asd ---
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :cl-user)

(defpackage :cl-hooks-system
  (:use
   :cl
   :asdf))

(in-package :cl-hooks-system)

(defsystem :cl-hooks
  :name        "cl-hooks"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.0.1"
  :license     "GPL3"
  :description "Type definitions for hooks"
  :components  ((:module "src"
		 :components ((:file "package")
			      (:file "conditions"
			       :depends-on ("package"))
			      (:file "hook"
			       :depends-on ("package" "conditions"))
			      (:file "state"
			       :depends-on ("package" "hook"))
			      (:file "symbol"
			       :depends-on ("package" "hook"))
			      (:file "object-internal"
			       :depends-on ("package" "hook"))
			      (:file "object-external"
			       :depends-on ("package" "hook"))
			      (:file "macros"
			       :depends-on ("package" "hook")))))
  :depends-on  (:alexandria
		:iterate
		:metabang-bind
		:closer-mop)
  :in-order-to ((test-op (test-op :cl-hooks-test))))

(defsystem :cl-hooks-test
  :name        "cl-hooks-test"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.0.1"
  :license     "GPL3"
  :description "Tests for hooks"
  :components  ((:module "test"
		 :components ((:file "package")
			      (:file "hooks"
			       :depends-on ("package")))))
  :depends-on  (:cl-hooks
		:lift)
  :in-order-to ((test-op (load-op :cl-hooks-test))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-hooks-test))))
  (let* ((suite-name (find-symbol "ROOT" :hooks.test))
	 (suite      (funcall (find-symbol "FIND-TESTSUITE" :lift) suite-name)))
    (funcall (find-symbol "RUN-TESTS" :lift) :suite suite)))
