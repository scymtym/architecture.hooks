;;; object-external.lisp ---
;;
;; Copyright (C) 2010 Jan Moringen
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
	  "Test adding handlers to object external hooks.")
  add-to-hook

  (let ((handler #'(lambda ())))
    (multiple-value-bind (handlers present?)
	(add-to-hook (external-hook object 'my-hook) handler)
      (ensure-same (length handlers) 1)
      (ensure (not present?)
	      :report "When adding a handler for the first time, the present? return value should be nil"))

    (multiple-value-bind (handlers present?)
	(add-to-hook (external-hook object 'my-hook) handler)
      (ensure-same (length handlers) 1)
      (ensure present?
	      :report "When adding a handler twice with :replace policy, the present? return value should be non-nil"))

    (ensure-condition duplicate-handler
      (add-to-hook (external-hook object 'my-hook) handler
		   :duplicate-policy :error))))

(addtest (object-external
	  :documentation
	  "Test clearing object external hooks.")
  clear-hook

  (add-to-hook (external-hook object 'my-hook) (lambda ()))
  (add-to-hook (external-hook object 'my-hook) (lambda ()))

  (clear-hook (external-hook object 'my-hook))
  (ensure-same (hook-handlers (external-hook object 'my-hook)) nil
	       :report "Found remaining handlers after clearing the hook"))
