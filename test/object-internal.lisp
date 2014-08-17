;;;; object-internal.lisp --- Unit tests for object internal hooks.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
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

(deftestsuite object-internal (root
                               hook-suite)
  ((object (make-instance '%hook-object/object-internal)))
  (:teardown
   (clear-hook (external-hook object 'my-hook)))
  (:run-setup :once-per-test-case)
  (:documentation
   "Tests for object-internal hooks."))

(addtest (object-internal
          :documentation
          "Test readers of object-internal hooks")
  readers

  (exercise-hook-readers (object-hook object 'my-hook)))

(addtest (object-internal
          :documentation
          "Test writers of object-internal hooks")
  writers

  (exercise-hook-writers (object-hook object 'my-hook)))

(addtest (object-internal
          :documentation
          "Ensure that retrieving a hook object twice yields `eq'
results.")
  retrieval-stability

  (ensure-same
   (object-hook object 'my-hook)
   (object-hook object 'my-hook)
   :test                    #'eq
   :ignore-multiple-values? t
   :report                  "Retrieving hook ~A twice should yield `eq' results, but did not."
   :arguments               ((object-hook object 'my-hook))))
