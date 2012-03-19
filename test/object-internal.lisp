;;; object-internal.lisp --- Unit tests for object internal hooks.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :hooks.test)

(defclass %hook-object/object-internal ()
  ((my-hook :initarg  :my-hook
	    :type     list
	    :initform nil))
  (:documentation
   "Instances of this class are used in unit tests for hooks which are
stored in objects."))

(deftestsuite object-internal (root
			       hook-suite)
  ((object (make-instance '%hook-object/object-internal)))
  (:teardown
   (clear-hook (external-hook object 'my-hook)))
  (:run-setup :once-per-test-case)
  (:documentation
   "Tests for object-internal hooks."))

(addtest (object-internal
          :documentation
	  "Test readers of object-internal hooks")
  readers

  (exercise-hook-readers (object-hook object 'my-hook)))

(addtest (object-internal
          :documentation
	  "Test writers of object-internal hooks")
  writers

  (exercise-hook-writers (object-hook object 'my-hook)))

(addtest (object-internal
          :documentation
	  "Ensure that retrieving a hook object twice yields `eq'
results.")
  retrieval-stability

  (ensure-same
   (object-hook object 'my-hook)
   (object-hook object 'my-hook)
   :test                    #'eq
   :ignore-multiple-values? t
   :report                  "Retrieving hook ~A twice should yield `eq' results, but did not."
   :arguments               ((object-hook object 'my-hook))))
