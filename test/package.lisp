;;;; package.lisp --- Package for hooks unit tests
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:hooks.test
  (:use
   #:cl
   #:hooks

   #:lift)

  (:export
   #:root)

  (:documentation
   "This package contains the unit tests for cl-hooks system."))

(cl:in-package #:hooks.test)

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
