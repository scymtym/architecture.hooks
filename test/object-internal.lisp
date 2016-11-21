;;;; object-internal.lisp --- Unit tests for object internal hooks.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:hooks.test)

(defclass %hook-object/object-internal ()
  ((my-hook :initarg  :my-hook
            :type     list
            :initform nil))
  (:documentation
   "Instances of this class are used in unit tests for hooks which are
    stored in objects."))

(def-fixture with-internal-hook-object ()
  (let ((object (make-instance '%hook-object/object-internal)))
    (unwind-protect
         (&body)
      (clear-hook (external-hook object 'my-hook)))))

(def-suite :hooks.object-internal
  :in :hooks
  :description
  "Tests for object-internal hooks.")
(in-suite :hooks.object-internal)

(test (object-internal.retrieval-stability
       :fixture with-internal-hook-object)
  "Ensure that retrieving a hook object twice yields `eq' results."

  (is (eq (object-hook object 'my-hook) (object-hook object 'my-hook))
      "Retrieving hook ~A twice should yield `eq' results, but did not."
      (object-hook object 'my-hook)))

(test (object-internal.readers
       :fixture with-internal-hook-object)
  "Test readers of object-internal hooks."

  (exercise-hook-readers (object-hook object 'my-hook)))

(test (object-internal.writers
       :fixture with-internal-hook-object)
  "Test writers of object-internal hooks."

  (exercise-hook-writers (object-hook object 'my-hook)))
