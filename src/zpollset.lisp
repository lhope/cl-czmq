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

;; MY OWN ADDITIONS.

;; really a zmq_pollitem_t
(cffi:defcstruct zpoller
  (socket :pointer)
  (fd :int)
  (events :short)
  (revents :short))

(defun zpollset-new (&rest socket/fd-events-list)
  "Creates a zpollset for use with zloop. socket/fd-events-list is a
list of (zsocket/fd . events).  zsocket/fd is a zsocket or an integer
file descriptor. Events is a list of one or more
of :zmq-pollin, :zmq-pollout or :zmq-pollerr."
  (let ((count (length socket/fd-events-list)))
    (cffi:foreign-alloc '(:struct zpoller) :count count
			:initial-contents
			(loop for (socket/fd . events) in socket/fd-events-list
			   for bitfield = (bitfield-options 'event-types events)
			   collect
			     (if (integerp socket/fd)
				 `(socket ,(cffi:null-pointer) fd ,socket/fd events ,bitfield)
				 `(socket ,socket/fd fd 0 events ,bitfield))))))

(defun zpollset-destroy (zpollset)
  "Destroy the zpollset."
  (unless (cffi:null-pointer-p zpollset)
    (cffi:foreign-free zpollset)))

(defmacro with-zpollset ((zpollset &rest socket/fd-events-list) &body body)
  "Create and bind a zpollset and execute body in that scope. Cleans
up the zpollset afterwards."
  `(let ((,zpollset
	  (zpollset-new
	   ,@(loop for socket/fd-events in socket/fd-events-list
		;; force evaluation
		collect `(list ,@socket/fd-events)))))
     (unwind-protect (progn ,@body)
       (zpollset-destroy ,zpollset))))

(defun zpollset-events (zpollset &optional (index 0))
  "Query the zpollset for triggered events."
  (let ((zpoller (cffi:mem-aref zpollset '(:struct zpoller) index)))
    (cffi:foreign-bitfield-symbols 'event-types (getf zpoller 'revents))))

(defun zpollset-poll (zpollset count timeout)
  "Call zmq_poll on the zpollset. count is the number of zpollers in
the zpollset we wish to poll.  If timeout is positive, wait that long
and then return. If it is -1, wait indefinitely.

Returns nil if the poll was interrupted, otherwise returns the number
of events triggered, which may be zero."
  (as-rc (cffi:foreign-funcall "zmq_poll"
			       :pointer zpollset
			       :int count
			       :long (* timeout ZMQ_POLL_MSEC) :int)))

