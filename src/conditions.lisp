;;; conditions.lisp --- Conditions used in the hooks library
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

(define-condition hook-error (simple-error)
  ((hook :initarg  :hook
	 :reader   hook-error-hook
	 :documentation
	 "The hook object from which the error originated."))
  (:documentation
   "This condition servers as a superclass for condition classes that
are related to hooks."))

(define-condition no-such-hook (hook-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "The hook ~S could not be found"
	     (hook-error-hook condition))))
  (:documentation
   "This condition is signaled when a designated hook cannot be
found."))

(define-condition duplicate-handler (hook-error)
  ((handler :initarg  :handler
	    :type     function
	    :reader   hook-error-handler
	    :documentation
	    "The handler which was added to the hook twice."))
  (:report
   (lambda (condition stream)
     (format stream "The handler ~S has already been added to the hook\
~S and the policy does not permit duplicate handlers"
	     (hook-error-handler condition)
	     (hook-error-hook    condition))))
  (:documentation
   "This condition is signaled if a handler is added to a hook to
which it has been added before and the policy does not permit handlers
to be added to a hook multiple times."))


;;; Conditions Related to Macros
;;

(define-condition malformed-hook-handler-binding (simple-error)
  ((binding :initarg  :binding
	    :reader   malformed-hook-handler-binding-binding
	    :documentation
	    "The invalid hook-handler binding."))
  (:report
   (lambda (condition stream)
     (format stream "Malformed hook-handler binding ~S"
	     (malformed-hook-handler-binding-binding condition))))
  (:documentation
   "This condition is signaled if an invalid hook-handler binding is
detected during the expansion of an `with-handlers' macro."))
