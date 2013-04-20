;;;; symbol.lisp --- Hooks that reside in variables.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :hooks)

(defmethod hook-name ((hook symbol))
  hook)

(defmethod hook-combination ((hook symbol))
  (get hook 'hook-combination 'cl:progn))

(defmethod (setf hook-combination) ((new-value t) (hook symbol))
  (setf (get hook 'hook-combination) new-value))

(defmethod hook-handlers ((hook symbol))
  (handler-case
      (symbol-value hook)
    (unbound-variable (condition)
      (declare (ignore condition))
      (error 'no-such-hook
	     :hook hook))))

(defmethod (setf hook-handlers) ((new-value list) (hook symbol))
  (setf (symbol-value hook) new-value))

(defmethod documentation ((hook symbol) (type (eql 'hook)))
  (or (get hook 'hook-documentation)
      (documentation hook 'variable)))

(defmethod (setf documentation) ((new-value string)
				 (hook      symbol)
				 (type      (eql 'hook)))
  (setf (get hook 'hook-documentation) new-value))
