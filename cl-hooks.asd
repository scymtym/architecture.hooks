;;;; cl-hooks.asd --- System definition for cl-hooks system.
;;;;
;;;; Copyright (C) 2010-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :cl-hooks
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING for details.
  :description "This system provides the hooks extension point
mechanism (as known, e.g., from GNU Emacs)."
  :depends-on  (:alexandria
                :let-plus
                :closer-mop
                :trivial-garbage)
  :components  ((:module     "src/early"
                 :pathname   "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "util")
                              (:file       "hook")
                              (:file       "state")
                              (:file       "mixins")
                              (:file       "symbol")
                              (:file       "object-internal")
                              (:file       "object-external")
                              (:file       "macros"))))

  :in-order-to ((test-op (test-op :cl-hooks-test))))

(defsystem :cl-hooks-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING for details.
  :description "Unit tests for the cl-hooks system."
  :depends-on  ((:version :cl-hooks (:read-file-form "version-string.sexp"))
                (:version :lift     "1.7.1"))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "symbol")
                              (:file       "object-external")
                              (:file       "object-internal")
                              (:file       "state")))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-hooks-test))))
  (funcall (find-symbol "RUN-TESTS" :lift) :config :generic))
