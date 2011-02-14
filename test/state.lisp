;;; state.lisp ---
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :hooks.test)

(deftestsuite state-root (object-hook-test)
  ((activate-events)
   (deactivate-events))
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
