;;;; hook.lisp --- Generic hook interface
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:hooks)

;;; Generic Implementation

(defmethod hook-combination ((hook t))
  'cl:progn)

(defmethod add-to-hook ((hook t) (handler function)
                        &key
                        (duplicate-policy :replace))
  (let+ ((present? (member handler (hook-handlers hook)))
         ((&flet add-it ()
            (push handler (hook-handlers hook))
            handler)))
    (ecase duplicate-policy
      ;; If HANDLER is already present, do nothing.
      (:do-nothing
       (if present?
           (values (hook-handlers hook) t)
           (values (add-it) nil)))

      ;; Add HANDLER, regardless of whether it is already present or
      ;; not.
      (:add
       (values (add-it) present?))

      ;; If HANDLER is already present, replace it. This has the
      ;; effect of moving HANDLER into the position of the most
      ;; recently added handler.
      (:replace
       (when present?
         ;; Do not use remove-from-hook to avoid running possibly
         ;; attached behavior.
         (removef (hook-handlers hook) handler))
       (values (add-it) present?))

      ;; When adding the same handler twice is not allowed and HANDLER
      ;; is already present, signal an error.
      (:error
       (when present?
         (error 'duplicate-handler
                :hook    hook
                :handler handler))
       (values (add-it) nil)))))

(defmethod remove-from-hook ((hook t) (handler function))
  (removef (hook-handlers hook) handler))

(defmethod clear-hook ((hook t))
  (setf (hook-handlers hook) nil))

(defmethod run-hook ((hook t) &rest args)
  (let+ (((&structure-r/o hook- handlers combination) hook)
         (result '())
         (current-handler)
         ((&labels run-handler (handler)
            (when-let ((values (multiple-value-list
                                (apply (the function handler) args))))
              (push values result))))
         ((&labels handle-error (condition)
            (signal-with-hook-and-handler-restarts
             hook current-handler condition
             (lambda ()
               (setf handlers (hook-handlers hook)
                     result   '())
               (throw 'abort-handler nil))
             (lambda (value)
               (return-from run-hook value))
             (lambda ()
               (push current-handler handlers)
               (throw 'abort-handler nil))
             (lambda ()
               (throw 'abort-handler nil))
             (lambda (value)
               (push (list value) result)
               (throw 'abort-handler nil))))))
    (declare (dynamic-extent #'run-handler #'handle-error))

    ;; Run all handlers and collect return values (if any).
    (when handlers
      ;; This slightly complicated logic ensures that the only things
      ;; we have to do in the common case are 1) binding the handler
      ;; 2) establishing the catch tag. Everything else only happens
      ;; when a handler actually signals an error.
      (handler-bind ((error #'handle-error))
        (do () ((null handlers))
          (catch 'abort-handler
            (do () ((null handlers))
              (setf current-handler (pop handlers))
              (run-handler current-handler)))))
      (setf result (nreverse result)))

    ;; Use hook combination to combine the list of results and form
    ;; the return value.
    (combine-results hook combination result)))

(declaim (inline run-hook-fast))

(defun run-hook-fast (hook &rest args)
  "Run HOOK with ARGS like `run-hook', with the following differences:
+ do not run any methods installed on `run-hook'
+ do not install any restarts
+ do not collect or combine any values returned by handlers."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; Run all handlers without restarts.
  (dolist (handler (hook-handlers hook))
    (apply #'run-handler-without-restarts handler args))
  (values))

(defmethod combine-results ((hook        t)
                            (combination (eql 'cl:progn))
                            (results     list))
  (apply #'values (lastcar results)))

(defmethod combine-results ((hook        t)
                            (combination function)
                            (results     list))
  (apply combination (mapcar #'first results)))
