;;; symbol.lisp --- Unit test for symbol hooks.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :hooks.test)

(defvar *my-hook* nil
  "A hook for tests.")

(deftestsuite symbol-root (root
			   hook-suite)
  ()
  (:documentation
   "Unit tests for symbol hooks."))

(addtest (symbol-root
          :documentation
	  "Test readers of symbol hooks")
  readers

  (exercise-hook-readers '*my-hook*))

(addtest (symbol-root
	  :documentation
	  "Test adding handlers to symbol hooks.")
  add-to-hook

  (let ((hook    '*my-hook*)
	(handler #'(lambda ())))
    (multiple-value-bind (added-handler present?)
	(add-to-hook hook handler)
      (ensure-same added-handler handler)
      (ensure-same (length (hook-handlers hook)) 1)
      (ensure (not present?)
	      :report "~@<When adding a handler for the first time, the ~
present? return value should be nil.~@:>"))

    (multiple-value-bind (added-handler present?)
	(add-to-hook hook handler)
      (ensure-same added-handler handler)
      (ensure-same (length (hook-handlers hook)) 1)
      (ensure present?
	      :report "~@<When adding a handler twice with :replace ~
policy, the present? return value should be non-nil.~@:>"))

    (ensure-condition duplicate-handler
      (add-to-hook hook handler
		   :duplicate-policy :error))))

(addtest (symbol-root
	  :documentation
	  "Test clearing symbol hooks.")
  clear-hook

  (let ((hook '*my-hook*))
    (add-to-hook hook (lambda ()))
    (add-to-hook hook (lambda ()))

    (clear-hook hook)
    (ensure-same (hook-handlers hook) nil
		 :report "~@<Found remaining handlers after clearing the
hook.~@:>")))
