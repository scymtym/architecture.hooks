;;;; package.lisp --- Package for hooks unit tests
;;;;
;;;; Copyright (C) 2010-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:hooks.test
  (:use
   #:cl
   #:hooks

   #:fiveam)

  (:export
   #:run-tests)

  (:documentation
   "This package contains the unit tests for cl-hooks system."))

(cl:in-package #:hooks.test)

(def-suite :hooks
  :description
  "Root unit test of the cl-hooks system.")

(defun run-tests ()
  (run! :hooks))

;;; Utilities

(defun exercise-hook-readers (hook)
  (is (typep (hook-name hook) 'symbol))
  (is (typep (hook-handlers hook) 'list))
  (is (typep (documentation hook t) '(or null string)))
  (is (stringp (princ-to-string hook))))

(defun exercise-hook-writers (hook)
  (setf (hook-handlers hook) (list (lambda ())))
  (is (= 1 (length (hook-handlers hook))))

  ;; Test modifying the hook's documentation.
  (setf (documentation hook 'hook) "foo")
  (is (string= "foo" (documentation hook 'hook))))
