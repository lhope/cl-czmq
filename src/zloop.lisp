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


;;//  @interface
;;//  Callback function for reactor events
;;typedef int (zloop_fn) (zloop_t *loop, zmq_pollitem_t *item, void *arg);


(defvar *int->arg*
  (make-hash-table :test 'eql))

(defconstant +max-zloop-arg+
  (1- (expt 2 16))
  "This should be lower than any actual addressable memory.")

(defun zloop-arg-new (arg)
  ;; null is reserved for nil.
  ;; args can be created multiple times.
  (unless arg (return-from zloop-arg-new (cffi:null-pointer)))
  (loop
     for count from 1
     for int = (let ((val (mod count +max-zloop-arg+)))
		 (assert (not (zerop val)) nil "zloop-arg-new: too many args!")
		 val)
     while (nth-value 1 (gethash int *int->arg*))
     finally
       (setf (gethash int *int->arg*) arg)
       (return (cffi:make-pointer int))))

(defun zloop-arg-destroy (int/pointer)
  (let ((int (if (integerp int/pointer)
		 int/pointer
		 (cffi:pointer-address int/pointer))))
    (remhash int *int->arg*)))

(defun zloop-arg (int/pointer)
  (let ((int (if (integerp int/pointer)
		 int/pointer
		 (cffi:pointer-address int/pointer))))
    ;; if not found, just return the pointer.
    (multiple-value-bind (val presentp)
	(gethash int *int->arg*)
      (if presentp val int/pointer))))

;; (cffi:defcallback zloop-fn :int ((zloop :pointer) (item :pointer) (arg :pointer)) ...)

(defmacro def-zloop-fn (fname (zloop pollitem arg) &body body)
  (let ((g!zloop (gensym))
	(g!pollitem (gensym))
	(g!c-arg (gensym))
	(g!err (gensym))
	(g!res (gensym)))
    `(cffi:defcallback ,fname :int ((,g!zloop :pointer) (,g!pollitem :pointer) (,g!c-arg :pointer))
       (multiple-value-bind (,g!res ,g!err)
	   (ignore-errors
	     (let ((,zloop ,g!zloop)
		   (,pollitem ,g!pollitem)
		   (,arg (zloop-arg ,g!c-arg)))
	       ,@body))
	 (if (or ,g!err (null ,g!res))
	     -1 0)))))

(defun zloop-new ()
  (as-pointer (zloop_new)))

(defun zloop-destroy (zloop)
  (with-foreign-box (&zloop zloop)
    (zmsg_destroy &zloop)
    (as-pointer (unbox &zloop :pointer))))

(defmacro with-zloop-args (args-and-values &body body)
  `(let ,(loop for (arg) in args-and-values
	    collect arg)
     (unwind-protect
	  (progn
	    ,@(loop for (arg value) in args-and-values
		 collect `(setf ,arg (zloop-arg-new ,value)))
	    ,@body)
       ,@(loop for (arg) in args-and-values
	    collect `(when ,arg (zloop-arg-destroy ,arg))))))

(defmacro with-zloop ((zloop &rest args-and-values) &body body)
  `(let ((,zloop (zloop-new)))
     (unwind-protect
	  (with-zloop-args (,@args-and-values)
	    ,@body)
       (zloop-destroy ,zloop))))

;;  Register pollitem with the reactor. When the pollitem is ready, will call
;;  the handler, passing the arg. Returns 0 if OK, -1 if there was an error.
;;  If you register the pollitem more than once, each instance will invoke its
;;  corresponding handler.
(defun zloop-poller (zloop item handler arg)
    (as-rc (zloop_poller zloop item (cffi:get-callback handler) (or arg (cffi:null-pointer)))))

;; TODO: http://zguide.zeromq.org/c:lruqueue3

;; Cancel a pollitem from the reactor, specified by socket or FD. If both
;; are specified, uses only socket. If multiple poll items exist for same
;; socket/FD, cancels ALL of them.
(defun zloop-poller-end (zloop pollitem)
  (zloop_poller_end zloop pollitem))

;; Register a timer that expires after some delay and repeats some number of
;; times. At each expiry, will call the handler, passing the arg. To
;; run a timer forever, use 0 times. Returns 0 if OK, -1 if there was an
;; error.
(defun zloop-timer (zloop delay times handler arg)
  (as-rc (zloop_timer zloop delay times (cffi:get-callback handler) (or arg (cffi:null-pointer)))))

(defun zloop-timer-end (zloop arg)
  (as-rc (zloop_timer_end zloop (or arg (cffi:null-pointer)))))

;; Set verbose tracing of reactor on/off
(defun zloop-set-verbose (zloop verbose)
  (zloop_set_verbose zloop (if verbose 1 0)))

;; Start the reactor. Takes control of the thread and returns when the 0MQ
;; context is terminated or the process is interrupted, or any event handler
;; returns -1. Event handlers may register new sockets and timers, and
;; cancel sockets. Returns 0 if interrupted, -1 if cancelled by a handler.
(defun zloop-start (zloop)
  (as-rc (zloop_start zloop)))

(def-zloop-fn s-timer-event (loop item output)
  (declare (ignore loop item))
  (zstr-send output "PING")
  t)

(def-zloop-fn s-socket-event (loop item arg)
  (declare (ignore loop item arg))
  nil)

(defun zloop-test (verbose)
  (format t " * zloop: ")

  ;;  @selftest
  (let* ((ctx (assert* (zctx-new)))
	 (output (assert* (zsocket-new ctx :zmq-pair)))
	 (input (assert* (zsocket-new ctx :zmq-pair))))
    (zsocket-bind output "inproc://zloop.test")
    (zsocket-connect input "inproc://zloop.test")

    (let ((loop (assert* (zloop-new))))
      (zloop-set-verbose loop verbose)

      ;; After 10 msecs, send a ping message to output
      (zloop-timer loop 10 1 's-timer-event output)

      ;; When we get the ping message, end the reactor
      (with-zpollset (poll-input (input :zmq-pollin))
	(assert (zloop-poller loop poll-input 's-socket-event nil))

	(zloop-start loop))

      (assert (null (zloop-destroy loop))))

    (zctx-destroy ctx))
  ;; @end
  (format t "OK~%"))
