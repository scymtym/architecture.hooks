;;; macros.lisp --- Convenience macros of the cl-hooks system.
;;
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
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

(cl:in-package :hooks)


;;; Activation Macros
;;

(defmacro defhook (hook &key combination documentation)
  "Instantiate HOOK and set COMBINATION and DOCUMENTATION."
  (with-unique-names (hook-var)
    `(let ((,hook-var ,hook))
       ,@(when combination
	   (list `(setf (hook-combination ,hook-var)
			,combination)))
       ,@(when documentation
	   (list `(setf (documentation ,hook-var 'hook)
			,documentation)))
       ,hook-var)))


;;;
;;

(defmacro define-hook-activation ((hook
				   &key
				   ((:var hook-var) (gensym "HOOK-VAR")))
				  activate &optional deactivate)
  "Execute [DE]ACTIVATE when HOOK becomes [in]active respectively.
HOOK is a form that designates or retrieves a hook. Examples include a
symbol designating a hook stored in that symbol or a form
like (object-hook OBJECT HOOK-SYMBOL).
Within the forms ACTIVATE and DEACTIVATE, the variable VAR (when
specified) can be used to refer to the hook object (not HOOK, which is
a form to retrieve the hook object)."
  `(let ((,hook-var ,hook))
     ,@(when activate
	 `((setf (slot-value ,hook-var 'on-become-active)
		 #'(lambda ()
		     ,activate))))
     ,@(when deactivate
	 `((setf (slot-value ,hook-var 'on-become-inactive)
		 #'(lambda ()
		     ,deactivate))))

     (when (hook-handlers ,hook-var)
       (on-become-active ,hook-var))))

(defmacro define-hook-activation-method ((hook
					  &key
					  ((:var hook-var) (gensym "HOOK-VAR")))
					 &body body)
  "When HOOK becomes active, execute BODY which has to return a method
object. When HOOK becomes inactive, the method is removed. The keyword
argument VAR can be used to specify the name of a variable to which
the hook object will be bound during the execution of BODY."
  `(let ((method))
     (define-hook-activation (,hook :var ,hook-var)
	 ;; Activate
	 (setf method (progn ,@body))
       ;; Deactivate
       (closer-mop::remove-method
	(closer-mop:method-generic-function method) method))))


;;; Activation Code for Internal Hooks
;;

(defmacro define-internal-hook-activation ((class hook
					    &key
					    ((:instance instance-var) (gensym))
					    ((:hook     hook-var)     (gensym)))
					   activate deactivate)
  "Execute ACTIVATE when internal HOOK of an instance of CLASS becomes
active and execute DEACTIVATE when such a hook becomes inactive. The
keyword arguments INSTANCE and HOOK can be used to name variables that
will be bound to the instance and the hook object respectively during
the execution of ACTIVATE and DEACTIVATE."
  `(defmethod initialize-instance :after ((,instance-var ,class)
					  &key)
     (define-hook-activation ((object-hook ,instance-var (quote ,hook))
			      :hook ,hook-var)
	 ,activate)
     ,deactivate))


;;; Activation Code for External Hooks
;;

(defmacro define-external-hook-activation ((name
					    &key
					    ((:object object-var) (gensym "OBJECT-VAR"))
					    ((:hook   hook-var)   (gensym "HOOK-VAR"))
					    (class                t))
					   &body body)
  "Execute BODY when the external hook named NAME becomes active.
The keyword arguments object and hook can be used to name variables
will be bound to the object and the hook object respectively during
the execution of BODY."
  `(defmethod external-hook :around ((,object-var ,class)
				     (hook        (eql (quote ,name))))
     ,(format nil "Add (de-)activation behavior to ~S for OBJECT." name)
     (multiple-value-bind (,hook-var present?) (call-next-method)
      (unless present?
	,@body)
      (values ,hook-var present?))))


;;;
;;

(defmacro with-handlers (hooks-and-handlers &body body)
  "Run BODY with handlers as specified in HOOKS-AND-HANDLERS.
HOOKS-AND-HANDLERS is a list of items of the form (HOOK HANDLER) where
HOOK is a hook and HANDLER is coercable to a function.
Example:
\(with-handlers (((object-external object 'hook)
                  (lambda (args)
                   (format t \"~S~%\" args))))
  (do-something))"
  ;; Check wellformedness of hook-handler bindings.
  (let ((binding (find-if-not (curry #'length= 2) hooks-and-handlers)))
    (when binding
      (error 'malformed-hook-handler-binding
	     :binding binding)))

  ;; Process hook-handler bindings.
  (iter (for (hook handler) in hooks-and-handlers)
	;; Collect a handler, install form and uninstall form.
	(let ((handler-var (gensym)))
	  (collect `(,handler-var (coerce ,handler 'function))
	    :into handlers)
	  (collect `(add-to-hook      ,hook ,handler-var)
	    :into install-hooks)
	  (collect `(remove-from-hook ,hook ,handler-var)
	    :into uninstall-hooks))

	;; Finally emit the code.
	(finally (return `(let (,@handlers)
			    ,@install-hooks
			    (unwind-protect
				 (progn ,@body)
			      ,@uninstall-hooks))))))
