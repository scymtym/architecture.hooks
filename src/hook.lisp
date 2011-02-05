;;; hook.lisp --- Generic hook interface
;;
;; Copyright (C) 2010, 2011 Jan Moringen
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

;; Hook implementations should also supply

(intern "HOOK")

;; + documentation ((hook TYPE) (type (eql 'hook)))
;; + (setf documentation) ((new-value t) (hook TYPE) (type (eql 'hook)))


;;; Generic Implementation
;;

(defmethod hook-combination ((hook t))
  'cl:progn)

(defmethod add-to-hook ((hook t) (handler function)
			&key (duplicate-policy :replace))
  (bind (((:flet add-it (handler present?))
	  (values (progn (push handler (hook-handlers hook))
			 handler)
		  present?))
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
	 (error 'duplicate-handler
		:hook    hook
		:handler handler))
       (add-it handler nil)))))

(defmethod remove-from-hook ((hook t) (handler function))
  (removef (hook-handlers hook) handler))

(defmethod clear-hook ((hook t))
  (setf (hook-handlers hook) nil))

(defmethod run-hook :around ((hook t) &rest args)
  (declare (ignore args))
  (with-hook-restarts hook
    (call-next-method)))

(defmethod run-hook ((hook t) &rest args)
  ;; Use hook combination to combine list of results and form return
  ;; value.
  (combine-results
   hook (hook-combination hook)
   ;; Run all handlers with restarts and collect the results (if any).
   (iter (for handler in (hook-handlers hook))
	 (let ((values (multiple-value-list
			(apply #'run-handler-with-restarts
			       handler args))))
	   (when values
	     (collect values))))))

(declaim (inline run-hook-fast))

(defun run-hook-fast (hook &rest args)
  "Run HOOK with ARGS like `run-hook', with the following differences:
+ do not run any methods installed on `run-hook'
+ do not install any restarts
+ do not collect or combine any values returned by handlers."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; Run all handlers without restarts.
  (iter (for handler in (hook-handlers hook))
	(apply #'run-handler-without-restarts handler args))
  (values))

(defmethod combine-results ((hook t) (combination (eql 'cl:progn)) (results list))
  (declare (ignore hook combination))

  (apply #'values (lastcar results)))

(defmethod combine-results ((hook t) (combination function) (results list))
  (declare (ignore hook))

  (apply combination (mapcar #'first results)))
