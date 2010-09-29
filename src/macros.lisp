;;; macros.lisp ---
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


;;;
;;

(defmacro define-per-instance-activated-hook
    ((class hook
      &key
      ((:instance instance-var) (gensym))
      ((:hook     hook-var)     (gensym)))
     activate deactivate)
  ""
  `(defmethod initialize-instance :around ((,instance-var ,class)
					   &rest initargs)
     (declare (ignore initargs))

     (call-next-method)

     (let ((,hook-var (object-hook ,instance-var (quote ,hook))))
       (defmethod on-become-active :after ((hook (eql ,hook-var)))
	 ,activate)
       (defmethod on-become-inactive :after ((hook (eql ,hook-var)))
	 ,deactivate))

     (when (hook-handlers ,hook-var)
       (on-become-active ,hook-var))))


;;;
;;

(defmacro with-handlers (hooks &body body)
  ""
  (let ((handlers)
	(install-hooks)
	(uninstall-hooks))
    (iter (for (hook handler) in hooks)
	  (let ((handler-var (gensym)))
	    (push `(,handler-var (coerce ,handler 'function)) handlers)
	    (push `(add-to-hook      ,hook ,handler-var) install-hooks)
	    (push `(remove-from-hook ,hook ,handler-var) uninstall-hooks)))
    `(let (,@handlers)
       ,@install-hooks
       (unwind-protect
	    (progn ,@body)
	 ,@uninstall-hooks))))
