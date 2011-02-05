;;; mixins.lisp --- Useful Mixin Classes for cl-hooks.
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