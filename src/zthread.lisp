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

;; This is a direct port of czmq/src/zthread.c which is
;;  Copyright (c) 1991-2013 iMatix Corporation <www.imatix.com>
;;  Copyright other contributors as noted in the AUTHORS file.

;;
;; @header
;;    The zthread class wraps OS thread creation. It creates detached threads
;;    that look like normal OS threads, or attached threads that share the
;;    caller's 0MQ context, and get an inproc pipe to talk back to the parent
;;    thread. Detached threads create their own 0MQ contexts as needed.
;; @discuss
;;    We have several use cases for multiple threads. One is to simulate many
;;    processes, so we can test 0MQ designs and flows more easily. Another is
;;    to create APIs that can send and receive 0MQ messages in the background.

;;    zthread solves these two use cases separately, using the zthread_new
;;    and zthead_fork methods respectively. These methods wrap the native
;;    system calls needed to start threads, so your code can remain fully
;    portable.

;;    Detached threads follow the POSIX pthreads API; they accept a void *
;;    argument and return a void * result (always NULL in our case).

;;    Attached thread receive a void * argument, a zctx_t context, and a pipe
;;    socket. The pipe socket is a PAIR socket that is connected back to the
;;    caller. When you call zthread_fork, it returns you a PAIR socket that
;;    is the other end of this pipe. Thus attached threads can talk back to
;;    their parent threads over the pipe. We use this very heavily when making
;;    so-called "asynchronous" APIs, which you can see in the Guide examples
;;    like 'clone'.

;;    To recap some rules about threading: do not share sockets between
;;    threads or your code will crash. You can migrate a socket from one
;;    thread to a child thread, if you stop using it in the parent thread
;;    immediately after creating the child thread. If you want to connect
;;    sockets over inproc:// they must share the same 0MQ context, i.e. be
;;    attached threads. You should always use zthread_fork to create an
;;    attached thread; it is not sufficient to pass a zctx_t structure to
;;    a detached thread (this will crash).

 ;;   If you want to communicate over ipc:// or tcp:// you may be sharing
 ;;   the same context, or use separate contexts. Thus, every detached thread
 ;;   usually starts by creating its own zctx_t instance.

(in-package :cl-czmq)


;;  --------------------------------------------------------------------------
;;  Create a detached thread. A detached thread operates autonomously
;;  and is used to simulate a separate process. It gets no ctx, and no
;;  pipe. Returns the resultant thread.

(defun zthread-new (thread-fn &rest args)
  (bordeaux-threads:make-thread
   (lambda ()
     (with-zsys-retry (t)
       (apply thread-fn args)))))


;;  --------------------------------------------------------------------------
;;  Create an attached thread. An attached thread gets a ctx and a PAIR
;;  pipe back to its parent. It must monitor its pipe, and exit if the
;;  pipe becomes unreadable. Returns pipe, or nil if there was an error.
;;  On success, returns the thread as a second value.
;;
;; Note the thread-fn has different argument order from czmq's zthread-fork -
;; (thread-fn ctx pipe &rest args) rather than (thread-fn args ctx pipe).

(defun zthread-fork (ctx thread-fn &rest args)
  ;;  Create our end of the pipe
  (let* ((pipe (zsocket-new ctx :zmq-pair))
	 (name (format nil "inproc://zctx-pipe-0x~8,'0x" (cffi:pointer-address pipe))))
    (if pipe
        (zsocket-bind pipe name)
	(return-from zthread-fork))

    ;;  Prepare argument shim for child thread
    (let* ((shim->ctx (or (zctx-shadow ctx)
			  (return-from zthread-fork)))
	   (shim->pipe (or (zsocket-new shim->ctx :zmq-pair)
			   (return-from zthread-fork))))

      ;;  Connect child pipe to our pipe
      (zsocket-connect shim->pipe name)

      (values pipe
	      (bordeaux-threads:make-thread
	       (lambda ()
		 (with-zsys-retry (t)
		   (apply thread-fn shim->ctx shim->pipe args)
		   (zctx-destroy shim->ctx)))
	       :name name)))))


;;  --------------------------------------------------------------------------
;;  Selftest

(defun s-test-detached (&rest args)
  (declare (ignore args))
  ;;  Create a socket to check it'll be automatically deleted
  (with-zctx (ctx)
    (assert ctx)

    (assert (zsocket-new ctx :zmq-push))))

(defun s-test-attached (ctx pipe &rest args)
  (declare (ignore args))
  ;;  Create a socket to check it'll be automatically deleted
  (zsocket-new ctx :ZMQ-PUSH)
  ;;  Wait for our parent to ping us, and pong back
  (zstr-recv pipe)
  (zstr-send pipe "pong"))

(defun zthread-test (verbose)
  (declare (ignore verbose))
  (format t " * zthread: ")

  ;;  @selftest
  (with-zctx (ctx)
    (assert ctx)

    ;;  Create a detached thread, let it run
    (assert (zthread-new #'s-test-detached))

    ;;  Create an attached thread, check it's safely alive
    (let ((pipe (zthread-fork ctx #'s-test-attached)))
      (assert pipe)
      (zstr-send pipe "ping")
      (let ((pong (zstr-recv pipe)))
	(assert (string= pong "pong")))))

  (format t "OK~%")
  0)
