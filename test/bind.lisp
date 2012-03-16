;;; bind.lisp --- Unit tests for the bind integration of cl-hooks.
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

(cl:in-package :hooks.test)

(deftestsuite bind (object-hook-test)
  ()
  (:documentation
   "Unit tests for hooks and bind integration"))

(addtest (bind
	  :documentation
	  "Smoke test for hooks and bind integration.")
  smoke

  (bind (((:handler (external-hook object 'my-hook) ())
	  1))
    (ensure-same (run-hook (external-hook object 'my-hook)) 1
		 :test #'=))

  (bind (((:handler (external-hook object 'my-hook) (x))
	  x))
    (ensure-same (run-hook (external-hook object 'my-hook) 1) 1
		 :test #'=))

  (bind (((:handler (external-hook object 'my-hook) ())
	  1)
	 ((:handler (external-hook object 'my-hook) ())
	  2))
    (ensure-same (run-hook (external-hook object 'my-hook)) 1
		 :test #'=
		 :report "Did not see the result of the least recently
added handler (~S)"
		 :arguments (1)))

  (bind (((:handler (external-hook object 'my-hook) (x))
	  x)
	 ((:handler (external-hook object 'my-hook) (x))
	  (- x)))
    (ensure-same (run-hook (external-hook object 'my-hook) 1) 1
		 :test #'=
		 :report "Did not see the result of the least recently
added handler (~S)"
		 :arguments (1))))
