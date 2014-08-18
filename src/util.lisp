;;;; util.lisp --- Utilities used by the hooks system.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:hooks)

(defun read-value ()
  "Read a replacement value."
  (format *query-io* "Replacement value: ")
  (force-output *query-io*)
  (list (read *query-io*)))

(declaim (inline run-handler-without-restarts))

(defun run-handler-without-restarts (handler &rest args)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "Run HANDLER with ARGS."
  (apply (the function handler) args))

(defun signal-with-hook-and-handler-restarts
    (hook handler condition
     retry-hook use-value-for-hook
     retry-handler skip-handler use-value-for-handler)
  "Signal CONDITION with appropriate restarts installed.

   The installed restarts for HANDLER are:
   + `retry'
   + `continue'
   + `use-value'

   The installed restarts for HOOK are:
   + `retry'
   + `use-value'"
  (restart-case
      (progn ; do not associate restarts
        (error condition))

    ;; Retry running the handler.
    (retry ()
      :report (lambda (stream)
                (format stream "~@<Retry running handler ~S.~:@>"
                        handler))
      (funcall retry-handler))

    ;; Skip the handler.
    (continue (&optional condition)
      :report (lambda (stream)
                (format stream "~@<Skip handler ~S.~:@>" handler))
      (declare (ignore condition))
      (funcall skip-handler))

    ;; Use a replacement value.
    (use-value (value)
      :report      (lambda (stream)
                     (format stream "~@<Specify a value to be used instead ~
                                     of the result of running handler ~
                                     ~S.~:@>"
                             handler))
      :interactive read-value
      (funcall use-value-for-handler value))

    ;; Retry running the hook.
    (retry ()
      :report (lambda (stream)
                (format stream "~@<Retry running hook ~S.~@:>"
                        hook))
      (funcall retry-hook))

    ;; Use a replacement value.
    (use-value (value)
      :report      (lambda (stream)
                     (format stream "~@<Specify a value instead of ~
                                     running hook ~S.~@:>"
                             hook))
      :interactive read-value
      (funcall use-value-for-hook value))))
