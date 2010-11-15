;;; object.lisp --- Hooks that reside in objects
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

(in-package :hooks)


;;; Object Hook Protocol
;;

(defgeneric object-hook (object hook)
  (:documentation
   "Return a representation of the slot residing in OBJECT under the
name HOOK."))


;;; Object Hook Class
;;

(defclass object-hook ()
  ((object      :initarg  :object
		:type     standard-object
		:documentation
		"The object in which the hook resides.")
   (slot        :initarg  :slot
		:type     symbol
		:documentation
		"The slot in which the hook resides.")
   (combination :initarg  :combination
		:accessor hook-combination
		:initform 'cl:progn
		:documentation
		""))
  (:documentation
   "Instances of this class represent hooks that reside in object."))

(defmethod hook-handlers ((hook object-hook))
  (with-slots (object slot) hook
    (handler-case
	(slot-value object slot)
      (error (condition)             ;; the precise condition should
	(declare (ignore condition)) ;; be something like (or
	                             ;; missing-slot unbound-slot)
	(error 'no-such-hook
	       :hook `(object-hook ,object ,slot))))))

(defmethod (setf hook-handlers) ((new-value list) (hook object-hook))
  (with-slots (object slot) hook
    (setf (slot-value object slot) new-value)))

(defmethod documentation ((hook object-hook) (type t))
  (declare (ignore type))

  (bind (((:slots object slot) hook)
	 (slot-object (find slot (closer-mop:class-slots (class-of object))
			    :key #'closer-mop:slot-definition-name)))
    (documentation slot-object t)))

(defmethod (setf documentation) ((new-value string) (hook object-hook) (type t))
  (declare (ignore type))

  (with-slots (object slot) hook
    (setf (documentation (closer-mop:class-slots (class-of object)) t)
	  new-value)))

(defmethod print-object ((object object-hook) stream)
  (with-slots (slot) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~A ~A (~A)"
	      slot
	      (hook-combination object)
	      (length (hook-handlers object))))))


;;; Implementation of the Object Hook Protocol
;;

(defmethod object-hook ((object standard-object) (hook symbol))
  (let ((table (or (get hook 'hook-objects)
		   (setf (get hook 'hook-objects)
			 (make-hash-table :test #'eq :weakness :key)))))
    (or (gethash object table)
	(setf (gethash object table)
	      (progn
		(handler-case
		    (slot-value object hook)
		  (error (condition)
		    (declare (ignore condition))

		    (error 'no-such-hook
			   :hook `(object-hook ,object ,hook))))
		(make-instance 'object-hook
			       :object object
			       :slot   hook))))))
