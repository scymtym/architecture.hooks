;;;; cl-hooks.asd --- System definition for cl-hooks system.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:cl-hooks-system
  (:use
   #:cl
   #:asdf))

(cl:in-package #:cl-hooks-system)

(when (asdf:find-system :asdf-system-connections nil)
  (asdf:load-system :asdf-system-connections))


;;; Hooks and Associated Test System
;;

(defsystem :cl-hooks
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.2.0"
  :license     "LLGPLv3; see COPYING for details."
  :description "This system provides the hooks extension point
mechanism (as known, e.g., from GNU Emacs)."
  :depends-on  (:alexandria
		:metabang-bind
		:closer-mop)
  :components  ((:module     "src/early"
		 :pathname   "src"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "conditions")
			      (:file       "protocol")
			      (:file       "util")))

		(:module     "src"
		 :depends-on ("src/early")
		 :components ((:file       "hook")
			      (:file       "state")
			      (:file       "mixins"
			       :depends-on ("state"))
			      (:file       "symbol")
			      (:file       "object-internal"
			       :depends-on ("mixins"))
			      (:file       "object-external"
			       :depends-on ("mixins"))
			      (:file       "macros"))))

  :in-order-to ((test-op (test-op :cl-hooks-test))))

(defsystem :cl-hooks-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.2.0"
  :license     "LLGPLv3; see COPYING for details."
  :description "Unit tests for the cl-hooks system."
  :depends-on  (:cl-hooks
		(:version :lift "1.7.1"))
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
			       :depends-on ("package"))))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-hooks-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))


;;; Hooks and Bind System Connection
;;

#+asdf-system-connections
(defsystem-connection :cl-hooks-and-bind
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.2.0"
  :license     "LLGPLv3; see LICENSE-LLGPL for details."
  :description "System connection that provides a binding form that
installs and uninstalls hook handlers around a form."
  :requires    (cl-hooks
		metabang-bind)
  :components  ((:file       "bind"
		 :pathname   "src/bind")))
