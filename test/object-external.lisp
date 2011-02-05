;;; object-external.lisp ---
;;
;; Copyright (C) 2010, 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program. If not, see
;; <http://www.gnu.org/licenses>.

(in-package :hooks.test)

(deftestsuite object-external (object-hook-test)
  ()
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
   :test   #'eq
   :report    "Retrieving hook ~A twice should yield `eq' results, but did not."
   :arguments ((external-hook object 'my-hook))))

(addtest (object-external
	  :documentation
	  "Test adding handlers to object external hooks.")
  add-to-hook

  (let ((hook    (external-hook object 'my-hook))
	(handler #'(lambda ())))
    (multiple-value-bind (added-handler present?)
	(add-to-hook hook handler)
      (ensure-same added-handler handler)
      (ensure-same (length (hook-handlers hook)) 1)
      (ensure (not present?)
	      :report "When adding a handler for the first time, the present? return value should be nil"))

    (multiple-value-bind (added-handler present?)
	(add-to-hook hook handler)
      (ensure-same added-handler handler)
      (ensure-same (length (hook-handlers hook)) 1)
      (ensure present?
	      :report "When adding a handler twice with :replace policy, the present? return value should be non-nil"))

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
		 :report "Found remaining handlers after clearing the hook")))
