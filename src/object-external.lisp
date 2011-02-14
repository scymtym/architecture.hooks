;;; object-external.lisp --- Hooks that reside outside of objects
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

(in-package :hooks)


;;; Object Hook Protocol
;;

(defgeneric external-hook (object hook)
  (:documentation
   "Return a representation of the slot residing outside of OBJECT
under the name HOOK."))


;;; Object Hook Class
;;

(defclass external-hook (internal-combination-mixin
			 internal-handlers-mixin
			 internal-documentation-mixin
			 simple-printing-mixin
			 activatable-mixin)
  ((name :initarg  :name
	 :type     symbol
	 :reader   hook-name
	 :documentation
	 "The name of this hook."))
  (:documentation
   "Instances of this class represent hooks that reside outside of
objects."))


;;; Implementation of the External Object Hook Protocol
;;

(defmethod external-hook ((object t) (hook symbol))
  (let* ((table  (or (get hook 'external-hook-objects)
		     (setf (get hook 'external-hook-objects)
			   (make-hash-table :test #'eq :weakness :key))))
	 (result (gethash object table)))
    (if result
	(values result t)
	(values (setf (gethash object table)
		      (make-instance 'external-hook
				     :name hook))
		nil))))
