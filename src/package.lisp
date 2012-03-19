;;; package.lisp --- Package definition for the hook library.
;;
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
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

(cl:defpackage :hooks
  (:use
   :cl
   :alexandria
   :iterate
   :bind)

  ;; Documentation Type Symbol
  (:export
   :hook)

  ;; Conditions
  (:export
   :hook-error

   :no-such-hook

   :duplicate-handler

   :malformed-hook-handler-binding)

  ;; Hook Protocol
  (:export
   :hook-name
   :hook-combination
   :hook-handlers

   :add-to-hook
   :remove-from-hook
   :clear-hook

   :run-hook
   :run-hook-fast

   :combine-results)

  ;; State Tracking Protocol
  (:export
   :on-become-active
   :on-become-inactive)

  ;; Object Hook Protocol
  (:export

   :object-hook
   :hook-object)

  ;; External Hook Protocol
  (:export
   :external-hook)

  ;; Convenience Marcos
  (:export
   :defhook

   :define-hook-activation

   :define-internal-hook-activation
   :define-external-hook-activation

   :define-hook-activation-method

   :with-handlers)

  (:documentation
   "This package contains functions and classes which implement the
\"hook\" extension point mechanism as known, for example, from GNU
Emacs.

Hooks are first-class objects which can be inspected and modified via
accessors:

+ `hook-name'
+ `hook-handlers'
+ `hook-combination'

+ `run-hook'

+ `add-to-hook'
+ `remove-from-hook'
+ `clear-hook'

The builtin kinds of hooks are the following

+ list of handlers as value of a symbol
+ list of handlers in an object slot
+ hook object is associated to arbitrary Lisp object"))
