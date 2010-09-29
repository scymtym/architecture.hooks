;;; hook.lisp --- Generic hook interface
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


;;; Hook Protocol
;;

(defgeneric hook-handlers (hook)
  (:documentation
   "Return a list of handlers attached to HOOK."))

(defgeneric (setf hook-handlers) (new-value hook)
  (:documentation
   "Replace list of handlers attached to HOOK with NEW-VALUE."))

(defgeneric add-to-hook (hook handler)
  (:documentation
   "Add HANDLER to HOOK."))

(defgeneric remove-from-hook (hook handler)
  (:documentation
   "Remove HANDLER from HOOK."))

(defgeneric run-hook (hook &rest args)
  (:documentation
   "Run HOOK passing extra args ARGS to all handlers."))


;;; Generic Implementation
;;

(defmethod add-to-hook ((hook t) (handler function))
  (push handler (hook-handlers hook)))

(defmethod remove-from-hook ((hook t) (handler function))
  (removef (hook-handlers hook) handler))

(defmethod run-hook ((hook t) &rest args)
  (dolist (handler (hook-handlers hook))
    (apply (the function handler) args)))
