;;;; object-external.lisp --- Hooks that reside outside of objects
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :hooks)


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
  (:default-initargs
   :name (required-argument :name))
  (:documentation
   "Instances of this class represent hooks that reside outside of
objects."))


;;; Implementation of the External Object Hook Protocol
;;

(defmethod external-hook ((object t) (hook symbol))
  (let ((table (or (get hook 'external-hook-objects)
		   (setf (get hook 'external-hook-objects)
			 (make-hash-table :test #'eq :weakness :key)))))
    (ensure-gethash object table (make-instance 'external-hook
						:name hook))))
