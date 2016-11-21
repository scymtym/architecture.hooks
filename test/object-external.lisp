;;;; object-external.lisp ---
;;;;
;;;; Copyright (C) 2010-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:hooks.test)

(defclass %hook-object/object-external ()
  ((my-hook :initarg  :my-hook
            :type     list
            :initform nil))
  (:documentation
   "Instances of this class are used in unit tests for hooks which are
    associated to objects."))

(def-fixture with-external-hook-object ()
  (let ((object (make-instance '%hook-object/object-external)))
    (unwind-protect
         (&body)
      (clear-hook (external-hook object 'my-hook)))))

(def-suite :hooks.object-external
  :in :hooks
  :description
  "Tests for object-external hooks.")
(in-suite :hooks.object-external)

(test (object-external.retrieval-stability
       :fixture with-external-hook-object)
  "Ensure that retrieving a hook object twice yields `eq' results."

  (is (eq (external-hook object 'my-hook) (external-hook object 'my-hook))
      "~@<Retrieving hook ~S twice should yield `eq' results, but did
       not.~@:>"
      (external-hook object 'my-hook)))

(test (object-external.readers
       :fixture with-external-hook-object)
  "Test readers of object external hooks."

  (exercise-hook-readers (external-hook object 'my-hook)))

(test (object-external.writers
       :fixture with-external-hook-object)
  "Test writers of object external hooks."

  (exercise-hook-writers (external-hook object 'my-hook)))

(test (object-external.add-to-hook
       :fixture with-external-hook-object)
  "Test adding handlers to object external hooks."

  (let ((hook    (external-hook object 'my-hook))
        (handler (lambda ())))
    (multiple-value-bind (added-handler present?)
        (add-to-hook hook handler)
      (is (eq handler added-handler))
      (is (= 1 (length (hook-handlers hook))))
      (is-false present?
                "~@<When adding a handler for the first time, the
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

(test (object-external.clear-hook
       :fixture with-external-hook-object)
  "Test clearing object external hooks."

  (let ((hook (external-hook object 'my-hook)))
    (add-to-hook hook (lambda ()))
    (add-to-hook hook (lambda ()))

    (clear-hook hook)
    (is (equal '() (hook-handlers hook))
        "~@<Found remaining handlers after clearing the hook.~@:>")))
