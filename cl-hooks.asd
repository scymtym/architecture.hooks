;;; cl-hooks.asd ---
;;
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
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

(cl:defpackage :cl-hooks-system
  (:use
   :cl
   :asdf))

(cl:in-package :cl-hooks-system)

(when (asdf:find-system :asdf-system-connections nil)
  (asdf:load-system :asdf-system-connections))


;;; Hooks and Associated Test System
;;

(defsystem :cl-hooks
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "LLGPL3; see LICENSE-LLGPL for details."
  :description "Type definitions for hooks"
  :depends-on  (:alexandria
		:iterate
		:metabang-bind
		:closer-mop)
  :components  ((:module     "src"
		 :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "util"
			       :depends-on ("package"))
			      (:file       "conditions"
			       :depends-on ("package"))
			      (:file       "hook"
			       :depends-on ("package" "util"
					    "conditions" "protocol"))
			      (:file       "state"
			       :depends-on ("package" "hook"))
			      (:file       "mixins"
			       :depends-on ("package" "hook"
					    "state"))
			      (:file       "symbol"
			       :depends-on ("package" "hook"))
			      (:file       "object-internal"
			       :depends-on ("package" "hook"
					    "mixins"))
			      (:file       "object-external"
			       :depends-on ("package" "hook"
					    "mixins"))
			      (:file       "macros"
			       :depends-on ("package" "hook")))))

  :in-order-to ((test-op (test-op :cl-hooks-test))))

(defsystem :cl-hooks-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "LLGPL3; see LICENSE-LLGPL for details."
  :description "Unit tests for the cl-hooks system."
  :depends-on  (:cl-hooks
		:lift)
  :components  ((:module     "test"
		 :components ((:file       "package")
			      (:file       "symbol"
			       :depends-on ("package"))
			      (:file       "object-external"
			       :depends-on ("package"))
			      (:file       "object-internal"
			       :depends-on ("package"))
			      (:file       "state"
			       :depends-on ("package"))
			      (:file       "bind"
			       :depends-on ("package")))))
  :in-order-to ((test-op (load-op :cl-hooks-test))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-hooks-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))


;;; Hooks and Bind System Connection
;;

#+asdf-system-connections
(defsystem-connection :cl-hooks-and-bind
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "LLGPL3; see LICENSE-LLGPL for details."
  :description "System connection that provides a binding form that
installs and uninstalls hook handlers around a form."
  :requires    (cl-hooks
		metabang-bind)
  :components  ((:module     "src"
		 :components ((:file "bind")))))
