;;; protocol.lisp ---  Protocol for the cl-hooks system.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :hooks)


;;; Hook Protocol
;;

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
;;
;; Implementations of hook kinds should define methods on
;; `cl:documentation' for documentation type `hooks:hook'.

(intern "HOOK")

;; + documentation ((hook TYPE) (type (eql 'hook)))
;; + (setf documentation) ((new-value t) (hook TYPE) (type (eql 'hook)))
