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

(defun zctx-new ()
  (zctx_new))

(defun zctx-destroy (zctx)
  (with-foreign-box (&zctx zctx)
    (zctx_destroy &zctx)
    (as-pointer (unbox &zctx :pointer))))

(defmacro with-zctx ((zctx) &body body)
  `(let ((,zctx (zctx-new)))
     (unwind-protect
	  (progn ,@body)
       (zctx-destroy ,zctx))))

(defun zctx-shadow (zctx)
  (as-pointer (zctx_shadow zctx)))

(defun zctx-set-iothreads (zctx iothreads)
  (zctx_set_iothreads zctx iothreads))

(defun zctx-set-linger (zctx linger)
  (zctx_set_linger zctx linger))

(defun zctx-set-hwm (zctx hwm)
  (zctx_set_hwm zctx hwm))

(defun zctx-hwm (zctx)
  (zctx_hwm zctx))

(defun zctx-underlying (zctx)
  (as-pointer (zctx_underlying zctx)))

(defun zctx-interrupted ()
  (not (zerop zctx_interrupted)))

;;CZMQ_EXPORT int
;;    zctx_test (bool verbose);

(defun zctx-test (verbose)
  (declare (ignore verbose))
  (format t " * zctx: ");

  ;;  @selftest
  ;;  Create and destroy a context without using it
  (let ((ctx (assert* (zctx-new))))
    (assert (null (zctx-destroy ctx))))

  ;; Create a context with many busy sockets, destroy it
  (let ((ctx (assert* (zctx-new))))
    (zctx-set-iothreads ctx 1)
    (zctx-set-linger ctx 5) ;; 5 msecs
    ;; using zsocket-new instead of zctx__socket_new
    (let ((s1 (zsocket-new ctx :zmq-pair))
	  (s2 (zsocket-new ctx :zmq-xreq))
	  (s3 (zsocket-new ctx :zmq-req))
	  (s4 (zsocket-new ctx :zmq-rep))
	  (s5 (zsocket-new ctx :zmq-pub))
	  (s6 (zsocket-new ctx :zmq-sub)))
      (zsocket-connect s1 "tcp://127.0.0.1:5555")
      (zsocket-connect s2 "tcp://127.0.0.1:5555")
      (zsocket-connect s3 "tcp://127.0.0.1:5555")
      (zsocket-connect s4 "tcp://127.0.0.1:5555")
      (zsocket-connect s5 "tcp://127.0.0.1:5555")
      (zsocket-connect s6 "tcp://127.0.0.1:5555")
      (assert (zctx-underlying ctx))
      (zctx-destroy ctx))
      ;; @end
    (format t "OK~%")
    0))
