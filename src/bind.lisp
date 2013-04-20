;;; bind.lisp --- Integration of cl-hooks and bind systems.
;;
;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :bind)

(defbinding-form (:handler
		  :use-values-p            nil
		  :accept-multiple-forms-p t
		  :docstring
		  "Add a handler for the specified hook while the body
executes.")
  ;; Check wellformedness of variables.
  (unless (and (alexandria:length= 2 variables)
	       (listp (second variables)))
    (error 'hooks:malformed-handler-binding
	   :binding variables))

  (destructuring-bind (hook lambda-list) variables
    `(hooks:with-handlers ((,hook #'(lambda ,lambda-list ,@values))))))
