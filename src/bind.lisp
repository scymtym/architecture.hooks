;;; bind.lisp --- Integration of cl-hooks and bind systems.
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

(in-package :bind)

(defbinding-form (:handler
		  :use-values-p            nil
		  :accept-multiple-forms-p t
		  :docstring
		  "Add a handler for the specified hook while the body
executes.")
  (destructuring-bind (hook lambda-list) variables
    `(hooks:with-handlers ((,hook #'(lambda ,lambda-list ,@values))))))
