;; Copyright (c) 2013, Lucas Hope <lucas.r.hope@gmail.com>.
;; Copyright other contributors as noted in the AUTHORS file.
;;
;; This file is part of cl-czmq - a re-binding of the C binding for
;; the zmq transport layer (czmq).
;;
;; This file is licensed under the terms of the LLGPL.
;;
;; This library is free software; you can redistribute it and/or modify
;; it under the terms of the Lisp Lesser General Public License version
;; 3, which consists of the GNU Lesser General Public License, either
;; version 3 or (at your option) any later version, as published by the
;; Free Software Foundation, and the Franz preamble.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; @header
;; The zclock class provides essential sleep and system time functions, used
;; to slow down threads for testing, and calculate timers for polling. Wraps
;; the non-portable system calls in a simple portable API.
;; @discuss
;; The Win32 Sleep() call defaults to 16ms resolution unless the system timer
;; resolution is increased with a call to timeBeginPeriod() permitting 1ms
;; granularity.
;; @end

(in-package :cl-czmq)

;;  --------------------------------------------------------------------------
;;  Sleep for a number of milliseconds

#-allegro
(defun zclock-sleep (msecs)
  (zclock_sleep msecs))

;; alternate zclock-sleep implementation for allegro.
#+allegro
(setf (sys::thread-control :clock-event-delta) 0)

#+allegro
(defun zclock-sleep (msecs)
  (sleep (* 0.001 msecs)))

;;  --------------------------------------------------------------------------
;;  Return current system clock as milliseconds

#-allegro
(defun zclock-time ()
  (cffi:foreign-funcall "zclock_time" :int64))

#+allegro
(defconstant +milli-multiplier+
  (/ 1000 internal-time-units-per-second))

#+allegro
(defun zclock-time ()
  "Note - this is not a real time, it is for use just as comparison with itself."
  (floor (* (get-internal-real-time) +milli-multiplier+)))

;;  --------------------------------------------------------------------------
;;  Print formatted string to stdout, prefixed by date/time and
;;  terminated with a newline.

(let (dst tz)
  (defun zclock-log (fmt &rest args)
    (unless tz
      (destructuring-bind (dst1 tz1)
	  (nthcdr 7 (multiple-value-list (decode-universal-time (get-universal-time))))
	(setf dst dst1 tz tz1)))
    (flet ((iso-8601-time ()
	     (multiple-value-bind (second minute hour date month year)
		 (decode-universal-time (get-universal-time) (- tz (if dst 1 0)))
	       (format nil "~D-~2,'0D-~2,'0D ~D:~2,'0D:~2,'0D"
		       year month date hour minute second))))
      (format t "~A " (iso-8601-time)))
    (apply #'format t fmt args)
    (fresh-line)))

(defun zclock-test (verbose)
  (declare (ignore verbose))
  (format t " * zclock: ")

  ;;  @selftest
  (let ((start (zclock-time)))
    (zclock-sleep 10)
    (assert (>= (- (zclock-time) start) 10))
    ;;  @end

    (format t "OK~%"))
  0)
