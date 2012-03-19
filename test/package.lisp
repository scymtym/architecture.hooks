;;; package.lisp --- Package for hooks unit tests
;;
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
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

(cl:defpackage :hooks.test
  (:use
   :cl
   :bind
   :hooks

   :lift)

  (:export
   :root)

  (:documentation
   "This package contains the unit tests for cl-hooks system."))

(cl:in-package :hooks.test)

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
     (ensure (typep (hook-name hook) 'symbol))
     (ensure (typep (hook-handlers hook) 'list))
     (ensure (typep (documentation hook t) '(or null string)))
     (ensure (stringp (princ-to-string hook)))))
  (:function
   (exercise-hook-writers (hook)
     (setf (hook-handlers hook) (list #'(lambda ())))
     (ensure-same (length (hook-handlers hook)) 1
		  :test #'=)

     ;; Test modifying the hook's documentation.
     (setf (documentation hook 'hook) "foo")
     (ensure-same (documentation hook 'hook) "foo"
		  :test #'string=)))
  (:documentation
   "Superclass for hook test suites."))
