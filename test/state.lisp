;;;; state.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :hooks.test)

(defclass %hook-object/state ()
  ((my-hook :initarg  :my-hook
	    :type     list
	    :initform nil))
  (:documentation
   "Instances of this class are used in unit tests for the hook state
mechanism."))

(deftestsuite state-root (root)
  ((activate-events)
   (deactivate-events)
   (object            (make-instance '%hook-object/state)))
  (:teardown
   (clear-hook (external-hook object 'my-hook)))
  (:run-setup :once-per-test-case)
  (:function
   (ensure-events (activate deactivate description)
     (ensure-same
      activate-events
      activate
      :test      #'equal
      :report    description
      :arguments (activate-events activate))
     (ensure-same
      deactivate-events
      deactivate
      :test      #'equal
      :report    description
      :arguments (deactivate-events deactivate))))
  (:documentation
   "Unit tests for hook activation and deactivation."))

(addtest (state-root
	  :documentation
	  "Smoke test for hook activation and deactivation.")
  smoke

  ;; We the hook (de)activate code is run, we push the hook object on
  ;; one of two lists. This way we know that the correct code is run
  ;; with the correct hook object.
  (let* ((hook (external-hook object 'my-hook)))
    (define-hook-activation (hook :var hook*)
	(push hook* activate-events)
      (push hook* deactivate-events))

    ;; Add one hook -> one activate event.
    (add-to-hook hook (lambda ()))
    (ensure-events
     `(,hook) ()
     "After adding one hook ~S, not ~S.")

    ;; Add another handler -> still only one activate event.
    (add-to-hook hook (lambda ()))
    (ensure-events
     `(,hook) ()
     "After adding another hook ~S, not ~S.")

    ;; Clear all handlers from the hook -> one deactivate event.
    (clear-hook hook)
    (ensure-events
     `(,hook) `(,hook)
     "After clearing the hook ~S, not ~S.")

    ;; Add one more handler -> another activate event.
    (add-to-hook hook (lambda ()))
    (ensure-events
     `(,hook ,hook) `(,hook)
     "After adding the handler a third time ~S, not ~S.")))
