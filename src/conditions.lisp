;;;; conditions.lisp --- Conditions used in the hooks library
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :hooks)

(define-condition hook-error-mixin (error)
  ((hook :initarg  :hook
	 :reader   hook-error-hook
	 :documentation
	 "The hook object from which the error originated."))
  (:default-initargs
   :hook (required-argument :hook))
  (:documentation
   "This condition servers as a superclass for condition classes that
are related to hooks."))

(define-condition no-such-hook (hook-error-mixin)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The hook ~S could not be found.~@:>"
	     (hook-error-hook condition))))
  (:documentation
   "This condition is signaled when a designated hook cannot be
found."))

(define-condition duplicate-handler (hook-error-mixin)
  ((handler :initarg  :handler
	    :type     function
	    :reader   hook-error-handler
	    :documentation
	    "The handler which was added to the hook twice."))
  (:default-initargs
   :handler (required-argument :handler))
  (:report
   (lambda (condition stream)
     (format stream "~@<The handler ~S has already been added to the ~
hook ~S and the policy does not permit duplicate handlers.~@:>"
	     (hook-error-handler condition)
	     (hook-error-hook    condition))))
  (:documentation
   "This condition is signaled if a handler is added to a hook to
which it has been added before and the policy does not permit handlers
to be added to a hook multiple times."))


;;; Conditions Related to Macros
;;

(define-condition malformed-handler-binding (error)
  ((binding :initarg  :binding
	    :reader   malformed-handler-binding-binding
	    :documentation
	    "The invalid hook-handler binding."))
  (:default-initargs
   :binding (required-argument :binding))
  (:report
   (lambda (condition stream)
     (format stream "~@<Malformed hook-handler binding ~S.~@:>"
	     (malformed-handler-binding-binding condition))))
  (:documentation
   "This condition is signaled if an invalid hook-handler binding is
detected during the expansion of an `with-handlers' macro."))
