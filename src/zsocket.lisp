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

(defconstant +zsocket-dynfrom+
  ZSOCKET_DYNFROM)

(defconstant +zsocket-dynto+
  ZSOCKET_DYNTO)

;;//  Callback function for zero-copy methods
;;typedef void (zsocket_free_fn) (void *data, void *arg);

(defun zsocket-new (zctx type)
  (zsocket_new zctx (cffi:foreign-enum-value 'socket-type type)))

(defun zsocket-destroy (zctx socket)
  (zsocket_destroy zctx socket))

(defmacro with-zsockets (zctx (&rest var-type-list) &body body)
  (let ((g!zctx (gensym)))
    `(let ((,g!zctx ,zctx))
       (let (,@(loop for (var type) in var-type-list
		  collect `(,var (zsocket-new ,g!zctx ,type))))
	 (unwind-protect (progn ,@body)
	   ,@(loop for (var) in var-type-list
		collect `(zsocket-destroy ,g!zctx ,var)))))))

(defun zsocket-bind (socket fmt &rest args)
  "Returns the bound port or nil."
  (as-rc (zsocket_bind socket "%s" :string (apply #'format nil fmt args))))

(defun zsocket-connect (socket fmt &rest args)
  (as-rc (zsocket_connect socket "%s" :string  (apply #'format nil fmt args))))

(defun zsocket-disconnect (socket fmt &rest args)
  (as-rc (zsocket_disconnect socket "%s" :string (apply #'format nil fmt args))))

(defun zsocket-poll (socket msecs)
  (as-bool (zsocket_poll socket msecs)))

(defun zsocket-type-str (socket)
  (zsocket_type_str socket))

;; need zmq_proxy somewhere. Here seems best.
;; always returns nil.
(defun zsocket-proxy (frontend backend &optional capture)
  (as-rc (cffi:foreign-funcall "zmq_proxy" :pointer frontend :pointer backend :pointer (or capture (cffi:null-pointer)) :int)))

#+ignore ;; not in 1.4.1
(defun zsocket-sendmem (socket data &rest zframe-options)
  "If data is a string, uses with-foreign-string. If
a (simple-array (unsigned-byte 8), uses
with-pointer-to-vector-data. If a vector of unsigned-bytes, copies
that vector using with-foreign-object. zsocket_sendmem makes a copy of
the data internally, so only the second option escapes multiple memory
copy."
  ;; with-foreign-data could support a list too.
  (with-foreign-bytes (bytes size data)
    (as-rc (zsocket_sendmem socket bytes size (bitfield-options 'zframe-options zframe-options)))))

;;CZMQ_EXPORT int
;;    zsocket_sendmem_zero_copy (void *socket, void *data, size_t size,
;;                               zsocket_free_fn *free_fn,
;;                               void *hint, int flags);


(defun zsocket-test (verbose)
  (declare (ignore verbose))
  (format t " * zsocket: ")

  ;; @selftest
  (let ((ctx (assert* (zctx-new))))

    ;;  Create a detached thread, let it run
    (let ((interf "*")
	  (domain "localhost")
	  (service 5560)
	  (writer (assert* (zsocket-new ctx :ZMQ-PUSH)))
	  (reader (assert* (zsocket-new ctx :ZMQ-PULL))))
      (assert (string= (zsocket-type-str writer) "PUSH"))
      (assert (string= (zsocket-type-str reader) "PULL"))
      (assert (eql service (zsocket-bind writer "tcp://~a:~d" interf service)))
      (assert (zerop (zsocket-connect reader "tcp://~a:~d" domain service)))

      (zstr-send writer "HELLO")
      (let ((message (zstr-recv reader)))
	(assert message)
	(assert (string= message "HELLO")))

      (let ((port (zsocket-bind writer "tcp://~a:*" interf)))
	(assert (and (>= port +zsocket-dynfrom+) (<= port +zsocket-dynto+))))

      (assert (not (zsocket-poll writer 100)))

      ;; deliberate typo (?)
      (assert (not (zsocket-connect reader "txp://~a:~d" domain service)))

      (zsocket-destroy ctx writer))
    (zctx-destroy ctx))
  ;;  @end

  (format t "OK~%")
  0)
