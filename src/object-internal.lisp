;;;; object.lisp --- Hooks that reside in objects
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:hooks)


;;; Object Hook Protocol
;;

(defgeneric object-hook (object hook)
  (:documentation
   "Return a representation of the slot residing in OBJECT under the
name HOOK."))


;;; Object Hook Class
;;

(defclass object-hook (internal-combination-mixin
		       simple-printing-mixin
		       activatable-mixin)
  ((object :initarg  :object
	   :type     standard-object
	   :reader   hook-object
	   :documentation
	   "The object in which the hook resides.")
   (slot   :initarg  :slot
	   :type     symbol
	   :reader   hook-name
	   :documentation
	   "The slot in which the hook resides."))
  (:default-initargs
   :object (required-argument :object)
   :slot   (required-argument :slot))
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

  (let+ (((&slots-r/o object slot) hook)
	 (slot-object (find slot (closer-mop:class-slots (class-of object))
			    :key #'closer-mop:slot-definition-name)))
    (documentation slot-object t)))

(defmethod (setf documentation) ((new-value string)
				 (hook      object-hook)
				 (type      t))
  (let+ (((&slots-r/o object slot) hook)
	 (slot-object (find slot (closer-mop:class-slots (class-of object))
			    :key #'closer-mop:slot-definition-name)))
    (setf (documentation slot-object t) new-value)))


;;; Implementation of the Object Hook Protocol
;;

(defmethod object-hook ((object standard-object) (hook symbol))
  (let ((table (or (get hook 'hook-objects)
		   (setf (get hook 'hook-objects)
			 (make-hash-table :test #'eq :weakness :key)))))
    (or (nth-value 0 (gethash object table))
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
