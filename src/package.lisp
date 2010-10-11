;;; package.lisp --- Package definition for the hook library.
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

(cl:in-package :cl-user)

(defpackage :hooks
  (:use
   :cl
   :alexandria
   :iterate
   :bind)
  (:export
   ;; Documentation Type Symbol
   :hook
   ;; Conditions
   :hook-error :no-such-hook :duplicate-handler
   ;; Hook Protocol
   :hook-combination :hook-handlers
   :add-to-hook :remove-from-hook :clear-hook
   :run-hook :combine-results
   ;; Object Hook Protocol
   :object-hook
   ;; External Hook Protocol
   :external-hook
   ;; State Tracking Protocol
   :on-become-active :on-become-inactive
   ;; Convenience Marcos
   :define-hook-activation :with-handlers))
