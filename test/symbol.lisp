;;;; symbol.lisp --- Unit test for symbol hooks.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:hooks.test)

(defvar *my-hook* nil
  "A hook for tests.")

(def-fixture with-symbol-hook ()
  (unwind-protect
       (&body)
    (clear-hook '*my-hook*)))

(def-suite :hooks.symbol-root
  :in :hooks
  :description
  "Unit tests for symbol hooks.")
(in-suite :hooks.symbol-root)

(test (readers :fixture with-symbol-hook)
  "Test readers of symbol hooks."

  (exercise-hook-readers '*my-hook*))

(test (writers :fixture with-symbol-hook)
  "Test writers of symbol hooks."

  (exercise-hook-writers '*my-hook*))

(test (add-to-hook :fixture with-symbol-hook)
  "Test adding handlers to symbol hooks."

  (let ((hook    '*my-hook*)
        (handler (lambda ())))
    (multiple-value-bind (added-handler present?)
        (add-to-hook hook handler)
      (is (eq handler added-handler))
      (is (= 1 (length (hook-handlers hook))))
      (is-false present?
                "~@<When adding a handler for the first time, the ~
                 present? return value should be nil.~@:>"))

    (multiple-value-bind (added-handler present?)
        (add-to-hook hook handler)
      (is (eq handler added-handler))
      (is (= 1 (length (hook-handlers hook))))
      (is-true present?
               "~@<When adding a handler twice with :replace policy, ~
                the present? return value should be non-nil.~@:>"))

    (signals duplicate-handler
      (add-to-hook hook handler :duplicate-policy :error))))

(test (clear-hook :fixture with-symbol-hook)
  "Test clearing symbol hooks."

  (let ((hook '*my-hook*))
    (add-to-hook hook (lambda ()))
    (add-to-hook hook (lambda ()))

    (clear-hook hook)
    (is (equal '() (hook-handlers hook))
        "~@<Found remaining handlers after clearing the hook.~@:>")))
