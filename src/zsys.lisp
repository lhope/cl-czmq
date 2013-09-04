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

;;  The zsys class provides a portable wrapper for miscellaneous functions
;;  that we want to wrap but which don't fit into any of the existing
;;  classes.

(in-package :cl-czmq)

(defvar *zsys-retry* nil
  "This variable is used to set the default behavior for recv
retries. Lisp has the issue that background threads can be interrupted
for garbage collecting at any time. We want to ignore these
interruptions. However the foreground thread is interrupted by
user. We don't want to ignore these! So we introduce this variable,
and default it to nil. zthread-new and zthread-fork bind it to
t. Either way, these bindings can be overridden in individual *-recv
functions.")

(defun zsys-set-retry (retry)
  "Set default recv retry to the given value.  Note that this is
rebound within zthread-new and zthread-fork."
  (setf *zsys-retry* retry))

(defun zsys-retry ()
  "Returns the current default recv retry."
  *zsys-retry*)

(defmacro with-zsys-retry ((retry) &body body)
  "Binds the default recv retry."
  `(let ((*zsys-retry* ,retry))
     ,@body))

(defun zsys-errno ()
  (let ((errno (cffi:foreign-funcall "zmq_errno" :int)))
    (values (cffi:foreign-enum-keyword 'error-code errno)
	    errno)))

(defun zsys-strerror ()
  (let ((errno (cffi:foreign-funcall "zmq_errno" :int)))
    (format nil "~a (~D=~a)"
	    (cffi:foreign-funcall "zmq_strerror" :int errno :string)
	    errno (cffi:foreign-enum-keyword 'error-code errno))))
