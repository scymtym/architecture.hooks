;;; package.lisp --- Package for hooks unit tests
;;
;; Copyright (C) 2010, 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program. If not, see
;; <http://www.gnu.org/licenses>.

(cl:in-package :cl-user)

(defpackage :hooks.test
  (:use
   :cl
   :bind
   :hooks
   :lift)
  (:export
   :root)
  (:documentation
   "This package contains the unit tests for cl-hooks system."))

(in-package :hooks.test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test of the cl-hooks system."))


;;;
;;

(deftestsuite hook-suite ()
  ()
  (:function
   (exercise-hook-readers (hook)
     (ensure
      (typep (hook-name hook) 'symbol))

     (ensure
      (typep (documentation hook t) '(or null string)))

     (ensure
      (stringp (princ-to-string hook)))))

  (:documentation
   "Superclass for hook test suites."))


;;;
;;

(defclass hook-object ()
  ((my-hook :initarg  :my-hook
	    :type     list
	    :initform nil
	    :documentation
	    "Dummy slot for internal hooks."))
  (:documentation
   "Instances of this class are used in unit tests for hooks which are
stored in or associated to objects."))

(deftestsuite object-hook-test (root)
  ((object (make-instance 'hook-object)))
  (:documentation
   "This class can be used as a superclass of test suites for hooks
that store information in or associate information to objects."))
