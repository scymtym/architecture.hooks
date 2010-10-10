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


;;; Generic Implementation
;;

(defmethod add-to-hook ((hook t) (handler function)
			&key (duplicate-policy :replace))
  (bind (((:flet add-it (handler present?))
	  (values (push handler (hook-handlers hook)) present?))
	 (present? (when (member handler (hook-handlers hook)) t)))
    (ecase duplicate-policy
      ;; If HANDLER is already present, do nothing.
      (:do-nothing
       (if present?
	   (values (hook-handlers hook) t)
	   (add-it handler nil)))

      ;; Add HANDLER, regardless of whether it is already present or
      ;; not.
      (:add
       (add-it handler present?))

      ;; If HANDLER is already present, replace it. This has the
      ;; effect of moving HANDLER into the position of the most
      ;; recently added handler.
      (:replace
       (when present?
	 ;; Do not use remove-from-hook to avoid running possibly
	 ;; attached behavior.
	 (removef (hook-handlers hook) handler))
       (add-it handler present?))

      ;; When adding the same handler twice is not allowed and HANDLER
      ;; is already present, signal an error.
      (:error
       (when present?
	 (error "Handler ~S already present and policy forbids adding a hook twice"
		handler))
       (add-it handler nil)))))

(defmethod remove-from-hook ((hook t) (handler function))
  (removef (hook-handlers hook) handler))

(defmethod clear-hook ((hook t))
  (setf (hook-handlers hook) nil))

(defmethod run-hook ((hook t) &rest args)
  (dolist (handler (hook-handlers hook))
    (apply (the function handler) args)))
