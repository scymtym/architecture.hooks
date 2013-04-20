;;;; mixins.lisp --- Useful Mixin Classes for cl-hooks.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :hooks)


;;; Internal Combination Mixin
;;

(defclass internal-combination-mixin ()
  ((combination :initarg  :combination
		:type     t
		:accessor hook-combination
		:initform 'cl:progn
		:documentation
		"The hook combination used by this hook."))
  (:documentation
   "This mixin adds a slot which stores the hook combination of the
hook."))


;;; Internal Handlers Mixin
;;

(defclass internal-handlers-mixin ()
  ((handlers :initarg  :handlers
	     :type     list
	     :accessor hook-handlers
	     :initform nil
	     :documentation
	     "The list of handlers associated with this hook."))
  (:documentation
   "This mixin adds a slot which stores the list of handlers of the
hook."))


;;; Internal Documentation Mixin
;;

(defclass internal-documentation-mixin ()
  ((documentation :initarg  :documentation
		  :type     (or null string)
		  :initform nil
		  :documentation
		  "Documentation string of the hook."))
  (:documentation
   "This mixin adds a slot which stores the documentation of the
hook."))

(defmethod documentation ((hook internal-documentation-mixin)
			  (type t))
  (declare (ignore type))

  (slot-value hook 'documentation))

(defmethod (setf documentation) ((new-value string)
				 (hook      internal-documentation-mixin)
				 (type      t))
  (declare (ignore type))

  (setf (slot-value hook 'documentation) new-value))


;;; Simple Printing Mixin
;;

(defclass simple-printing-mixin ()
  ()
  (:documentation
   "This mixin adds simple printing behavior for hooks."))

(defmethod print-object ((object simple-printing-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A (~A)"
	    (hook-name object)
	    (hook-combination object)
	    (length (hook-handlers object)))))


;;; Activatable Mixin
;;

(defclass activatable-mixin ()
  ((on-become-active   :initarg  on-become-active
		       :type     (or null function)
		       :initform nil
		       :documentation
		       "If a function is stored in this slot, the
function is called when the hook becomes active. ")
   (on-become-inactive :initarg   on-become-inactive
		       :type     (or null function)
		       :initform nil
		       :documentation
		       "If a function is stored in this slot, the
function is called when the hook becomes inactive."))
  (:documentation
   "This mixin adds slot to functions which run when the hook becomes
active or inactive."))

(defmethod on-become-active ((hook activatable-mixin))
  "If HOOK has a handler for becoming active installed, call that
handler."
  (when-let ((on-become-active (slot-value hook 'on-become-active)))
    (funcall (the function on-become-active))))

(defmethod on-become-inactive ((hook activatable-mixin))
  "If HOOK has a handler for becoming inactive installed, call that
handler."
  (when-let ((on-become-inactive (slot-value hook 'on-become-inactive)))
    (funcall (the function on-become-inactive))))
