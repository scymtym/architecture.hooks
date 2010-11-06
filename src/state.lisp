;;; state.lisp --- Functionality for tracking the state of hooks.
;;
;; Copyright (C) 2010 Jan Moringen
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

(in-package :hooks)


;;; Hook Activity Tracking Protocol
;;

(defgeneric on-become-active (hook)
  (:documentation
   "Called when HOOK becomes active."))

(defgeneric on-become-inactive (hook)
  (:documentation
   "Called when HOOK becomes inactive."))


;;; Default Implementation of the Activity Tracking Protocol
;;

(defmethod (setf hook-handlers) :around ((new-value list) (hook t))
  "Check whether HOOK becomes active or inactive."
  (declare (ignore new-value))

  (let ((was-empty?     (null (hook-handlers hook)))
	(will-be-empty? (null new-value))
	(result         (call-next-method)))
    (cond
      ((and was-empty? (not will-be-empty?))
       (on-become-active hook))
      ((and (not was-empty?) will-be-empty?)
       (on-become-inactive hook)))
    result))

(defmethod on-become-active ((hook t))
  "Default behavior is to do nothing."
  (declare (ignore hook))
  (values))

(defmethod on-become-inactive ((hook t))
  "Default behavior is to do nothing."
  (declare (ignore hook))
  (values))
