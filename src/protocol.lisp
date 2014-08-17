;;;; protocol.lisp ---  Protocol for the cl-hooks system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:hooks)

;;; Hook Protocol

(defgeneric hook-name (hook)
  (:documentation
   "Return the name of HOOK (a symbol)."))

(defgeneric hook-combination (hook)
  (:documentation
   "Return the hook combination used by HOOK."))

(defgeneric (setf hook-combination) (new-value hook)
  (:documentation
   "Install NEW-VALUE as the hook combination used by HOOK."))

(defgeneric hook-handlers (hook)
  (:documentation
   "Return a list of handlers attached to HOOK."))

(defgeneric (setf hook-handlers) (new-value hook)
  (:documentation
   "Replace list of handlers attached to HOOK with NEW-VALUE."))

(defgeneric add-to-hook (hook handler &key duplicate-policy)
  (:documentation
   "Add HANDLER to HOOK."))

(defgeneric remove-from-hook (hook handler)
  (:documentation
   "Remove HANDLER from HOOK."))

(defgeneric clear-hook (hook)
  (:documentation
   "Remove all handlers from HOOK."))

(defgeneric run-hook (hook &rest args)
  (:documentation
   "Run HOOK passing extra args ARGS to all handlers."))

(defgeneric combine-results (hook combination results)
  (:documentation
   "Combine RESULTS of running HOOK's handlers according to COMBINATION."))

;;; Hook documentation
;;;
;;; Implementations of hook kinds should define methods on
;;; `cl:documentation' for documentation type `hooks:hook'.

(intern "HOOK")

;; + documentation ((hook TYPE) (type (eql 'hook)))
;; + (setf documentation) ((new-value t) (hook TYPE) (type (eql 'hook)))
