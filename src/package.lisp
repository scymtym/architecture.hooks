;;;; package.lisp --- Package definition for the hook library.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:hooks
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  ;; Documentation Type Symbol
  (:export
   #:hook)

  ;; Conditions
  (:export
   #:hook-error-mixin
   #:hook-error-hook

   #:hook-error

   #:no-such-hook

   #:duplicate-handler
   #:hook-error-handler

   #:malformed-handler-binding
   #:malformed-handler-binding-binding)

  ;; Hook Protocol
  (:export
   #:hook-name
   #:hook-combination
   #:hook-handlers

   #:add-to-hook
   #:remove-from-hook
   #:clear-hook

   #:run-hook
   #:run-hook-fast

   #:combine-results)

  ;; State Tracking Protocol
  (:export
   #:on-become-active
   #:on-become-inactive)

  ;; Object Hook Protocol
  (:export

   #:object-hook
   #:hook-object)

  ;; External Hook Protocol
  (:export
   #:external-hook)

  ;; Convenience Marcos
  (:export
   #:defhook

   #:define-hook-activation

   #:define-internal-hook-activation
   #:define-external-hook-activation

   #:define-hook-activation-method

   #:with-handlers)

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
