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

(defun zclock-sleep (msecs)
  (zclock_sleep msecs))

;;  --------------------------------------------------------------------------
;;  Return current system clock as milliseconds

(defun zclock-time ()
  "Note - not same as (get-universal-time)."
  (zclock_time))

;;  --------------------------------------------------------------------------
;;  Print formatted string to stdout, prefixed by date/time and
;;  terminated with a newline.

(defun zclock-log (fmt &rest args)
  (zclock_log "%s" :string (apply #'format nil fmt args)))

;;  --------------------------------------------------------------------------
;;  Self test of this class

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
