;;;; state.lisp ---
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:hooks.test)

(defclass %hook-object/state ()
  ((my-hook :initarg  :my-hook
            :type     list
            :initform nil))
  (:documentation
   "Instances of this class are used in unit tests for the hook state
    mechanism."))

(def-fixture with-state-object ()
  (let ((activate-events   '())
        (deactivate-events '())
        (object            (make-instance '%hook-object/state)))
    (unwind-protect
         (&body)
      (clear-hook (external-hook object 'my-hook)))))

(defun ensure-events (expected-activate   activate-events
                      expected-deactivate deactivate-events
                      description)
  (is (equal expected-activate activate-events)
      description activate-events expected-activate)
  (is (equal expected-deactivate deactivate-events)
      description deactivate-events expected-deactivate))

(def-suite :hooks.state
  :in :hooks
  :description
  "Unit tests for hook activation and deactivation.")
(in-suite :hooks.state)

(test (smoke :fixture with-state-object)
  "Smoke test for hook activation and deactivation."

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
     `(,hook) activate-events () deactivate-events
     "After adding one hook ~S, not ~S.")

    ;; Add another handler -> still only one activate event.
    (add-to-hook hook (lambda ()))
    (ensure-events
     `(,hook) activate-events () deactivate-events
     "After adding another hook ~S, not ~S.")

    ;; Clear all handlers from the hook -> one deactivate event.
    (clear-hook hook)
    (ensure-events
     `(,hook) activate-events `(,hook) deactivate-events
     "After clearing the hook ~S, not ~S.")

    ;; Add one more handler -> another activate event.
    (add-to-hook hook (lambda ()))
    (ensure-events
     `(,hook ,hook) activate-events `(,hook) deactivate-events
     "After adding the handler a third time ~S, not ~S.")))
