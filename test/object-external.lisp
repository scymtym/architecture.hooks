;;;; object-external.lisp ---
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :hooks.test)

(defclass %hook-object/object-external ()
  ((my-hook :initarg  :my-hook
	    :type     list
	    :initform nil))
  (:documentation
   "Instances of this class are used in unit tests for hooks which are
associated to objects."))

(deftestsuite object-external (root
			       hook-suite)
  ((object (make-instance '%hook-object/object-external)))
  (:teardown
   (clear-hook (external-hook object 'my-hook)))
  (:run-setup :once-per-test-case)
  (:documentation
   "Tests for object-external hooks."))

(addtest (object-external
          :documentation
	  "Ensure that retrieving a hook object twice yields `eq'
results.")
  retrieval-stability

  (ensure-same
   (external-hook object 'my-hook)
   (external-hook object 'my-hook)
   :test                    #'eq
   :ignore-multiple-values? t
   :report                  "~@<Retrieving hook ~S twice should ~
yield `eq' results, but did not.~@:>"
   :arguments               ((external-hook object 'my-hook))))

(addtest (object-external
          :documentation
	  "Test readers of object external hooks")
  readers

  (exercise-hook-readers (external-hook object 'my-hook)))

(addtest (object-external
          :documentation
	  "Test writers of object external hooks")
  writers

  (exercise-hook-writers (external-hook object 'my-hook)))

(addtest (object-external
	  :documentation
	  "Test adding handlers to object external hooks.")
  add-to-hook

  (let ((hook    (external-hook object 'my-hook))
	(handler #'(lambda ())))
    (multiple-value-bind (added-handler present?)
	(add-to-hook hook handler)
      (ensure-same added-handler handler)
      (ensure-same (length (hook-handlers hook)) 1 :test #'=)
      (ensure (not present?)
	      :report "~@<When adding a handler for the first time, the ~
present? return value should be nil.~@:>"))

    (multiple-value-bind (added-handler present?)
	(add-to-hook hook handler)
      (ensure-same added-handler handler)
      (ensure-same (length (hook-handlers hook)) 1 :test #'=)
      (ensure present?
	      :report "~@<When adding a handler twice with :replace ~
policy, the present? return value should be non-nil.~@:>"))

    (ensure-condition duplicate-handler
      (add-to-hook hook handler
		   :duplicate-policy :error))))

(addtest (object-external
	  :documentation
	  "Test clearing object external hooks.")
  clear-hook

  (let ((hook (external-hook object 'my-hook)))
    (add-to-hook hook (lambda ()))
    (add-to-hook hook (lambda ()))

    (clear-hook hook)
    (ensure-same (hook-handlers hook) nil
		 :report "~@<Found remaining handlers after clearing the
hook.~@:>")))
