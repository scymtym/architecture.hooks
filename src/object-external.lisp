;;; object-external.lisp --- Hooks that reside outside of objects
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


;;; Object Hook Protocol
;;

(defgeneric external-hook (object hook)
  (:documentation
   "Return a representation of the slot residing outside of OBJECT
under the name HOOK."))


;;; Object Hook Class
;;

(defclass external-hook ()
  ((handlers :initarg  :handlers
	     :type     list
	     :initform nil
	     :documentation
	     ""))
  (:documentation
   "Instances of this class represent hooks that reside outside of
objects."))

(defmethod hook-handlers ((hook external-hook))
  (slot-value hook 'handlers))

(defmethod (setf hook-handlers) ((new-value list) (hook external-hook))
  (setf (slot-value hook 'handlers) new-value))

(defmethod print-object ((object external-hook) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~A)" (length (hook-handlers object)))))


;;; Implementation of the External Object Hook Protocol
;;

(defmethod external-hook ((object standard-object) (hook symbol))
  (let ((table (or (get hook 'external-hook-objects)
		   (setf (get hook 'external-hook-objects)
			 (make-hash-table :test #'eq :weakness :key)))))
    (or (gethash object table)
	(setf (gethash object table)
	      (make-instance 'external-hook)))))