;;; macros.lisp --- Convenience macros of the cl-hooks system.
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


;;; Activation Macros
;;

(defmacro define-hook-activation ((hook &key ((:var hook-var) (gensym)))
				  activate deactivate)
  "Execute [DE]ACTIVATE when HOOK becomes [in]active respectively.
HOOK is a form that designates or retrieves a hook. Examples include a
symbol designating a hook stored in that symbol or a form
like (object-hook OBJECT HOOK-SYMBOL).
Within the forms ACTIVATE and DEACTIVATE, the variable VAR (when
specified) can be used to refer to the hook object (not HOOK, which is
a form to retrieve the hook object)."
  (once-only (hook)
    `(progn
       (defmethod on-become-active :after ((,hook-var (eql ,hook)))
	 ,activate)
       (defmethod on-become-inactive :after ((,hook-var (eql ,hook)))
	 ,deactivate)

       (when (hook-handlers ,hook)
	 (on-become-active ,hook)))))

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
    (error 'malformed-hook-handler-binding
	   :binding binding))

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
