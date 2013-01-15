;;;; state.lisp --- Functionality for tracking the state of hooks.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:hooks)


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
  (values))

(defmethod on-become-inactive ((hook t))
  "Default behavior is to do nothing."
  (values))
