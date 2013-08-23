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

(in-package :cl-czmq)

(defun %zstr-recv (socket)
  (with-freed-string
    (cffi:foreign-funcall "zstr_recv" :pointer socket :pointer)))

;; retrying zstr-recv
(defun zstr-recv (socket &optional (retry *zsys-retry*))
  (loop for str = (%zstr-recv socket)
     when (or str (not retry) (not (eql (zsys-errno) :eintr)))
     return str))

(defun zstr-recv-nowait (socket)
  (with-freed-string
    (cffi:foreign-funcall "zstr_recv_nowait" :pointer socket :pointer)))

(defun zstr-send (socket fmt &rest args)
  (as-rc (zstr_send socket "%s" :string (apply #'format nil fmt args))))

(defun zstr-sendm (socket fmt &rest args)
  (as-rc (zstr_sendm socket "%s" :string (apply #'format nil fmt args))))

;;  --------------------------------------------------------------------------
;;  Selftest

(defun zstr-test (verbose)
  (declare (ignore verbose))
  (format t " * zstr: ")

  ;;  @selftest
  (let* ((ctx (assert* (zctx-new)))
	 (output (assert* (zsocket-new ctx :zmq-pair)))
	 (input (assert* (zsocket-new ctx :zmq-pair))))
    (zsocket-bind output "inproc://zstr.test")
    (zsocket-connect input "inproc://zstr.test")

    ;;  Send ten strings, five strings with MORE flag and then END
    (dotimes (string-nbr 10)
      (zstr-send output "this is string ~d" string-nbr))
    (dotimes (string-nbr 5)
      (zstr-sendm output "this is string ~d" string-nbr))
    (zstr-send output "END")

    ;;  Read and count until we receive END
    (loop for string-nbr from 0
       for string = (zstr-recv input)
       until (string= string "END")
       finally 
	 (assert (= string-nbr 15)))

    (zctx-destroy ctx))
  ;;  @end

  (format t "OK~%")
  0)
