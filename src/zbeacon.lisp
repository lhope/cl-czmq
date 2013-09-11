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

;;  Create a new beacon on a certain UDP port
(defun zbeacon-new (port-nbr)
  (as-pointer (zbeacon_new port-nbr)))

;;  Destroy a beacon
(defun zbeacon-destroy (zbeacon)
  "Returns the new value of zbeacon (should be null pointer)"
  (with-foreign-box (&zbeacon zbeacon)
    (zbeacon_destroy &zbeacon)
    (as-pointer (unbox &zbeacon :pointer))))

;;  Return our own IP address as printable string
;;  (note this is not copied, so no need to free.
(defun zbeacon-hostname (zbeacon)
  (zbeacon_hostname zbeacon))

;;  Set broadcast interval in milliseconds (default is 1000 msec)
(defun zbeacon-set-interval (zbeacon interval)
  (zbeacon_set_interval zbeacon interval)
  (values))

;;  Filter out any beacon that looks exactly like ours
(defun zbeacon-noecho (zbeacon)
  (zbeacon_noecho zbeacon)
  (values))

;;  Start broadcasting beacon to peers at the specified interval
(defun zbeacon-publish (zbeacon transmit)
  (with-foreign-bytes (bytes size transmit)
    (zbeacon_publish zbeacon bytes size))
  (values))

;;  Stop broadcasting beacons
(defun zbeacon-silence (zbeacon)
  (zbeacon_silence zbeacon)
  (values))

;;  Start listening to other peers; zero-sized filter (or null)  means get everything
(defun zbeacon-subscribe (zbeacon filter)
  (if filter
      (with-foreign-bytes (bytes size filter)
	(zbeacon_subscribe zbeacon bytes size))
      (zbeacon_subscribe zbeacon (cffi:null-pointer) 0))
  (values))

;;  Stop listening to other peers
(defun zbeacon-unsubscribe (zbeacon)
  (zbeacon_unsubscribe zbeacon)
  (values))

;;  Get beacon pipe, for polling or receiving messages
(defun zbeacon-pipe (zbeacon)
  (as-pointer (zbeacon_pipe zbeacon)))

;;  --------------------------------------------------------------------------
;;  Self test of this class

(defun zbeacon-test (verbose)
  (declare (ignore verbose))
  (format t " * zbeacon: ")

  ;;  Basic test: create a service and announce it
  (let* ((ctx (zctx-new))
	 ;;  Create a service socket and bind to an ephemeral port
	 (service (zsocket-new ctx :zmq-pub))
	 (port-nbr (zsocket-bind service "tcp://*:*"))
	 ;;  Create beacon to broadcast our service
	 (announcement (make-array 2
				   :element-type '(unsigned-byte 8)
				   :initial-contents (list (logand (ash port-nbr -8) #xff)
							   (logand port-nbr #xff))))
	 (service-beacon (zbeacon-new 9999)))
    (zbeacon-set-interval service-beacon 100)
    (zbeacon-publish service-beacon announcement)

    ;;  Create beacon to lookup service
    (let ((client-beacon (zbeacon_new 9999)))
      (zbeacon-subscribe client-beacon nil)

      ;;  Wait for at most 1/2 second if there's no broadcast networking
      (zsocket-set-rcvtimeo (zbeacon-pipe client-beacon) 500)

      (let ((ipaddress (zstr-recv (zbeacon-pipe client-beacon))))
	(when (assert* ipaddress)
	  (let* ((content (zframe-recv (zbeacon-pipe client-beacon)))
		 (zframe-data (zframe-data content))
		 (received-port (+ (ash (aref zframe-data 0) 8)
				   (aref zframe-data 1))))
	    (assert (= received-port port-nbr)))))
      (zbeacon-destroy client-beacon))
    (zbeacon-destroy service-beacon)
    (zctx-destroy ctx))

  ;;  @selftest
  (let ((node1 (zbeacon-new 5670))
	(node2 (zbeacon-new 5670))
	(node3 (zbeacon-new 5670)))
    (assert (zbeacon-hostname node1))
    (assert (zbeacon-hostname node2))
    (assert (zbeacon-hostname node3))

    (zbeacon-set-interval node1 250)
    (zbeacon-set-interval node2 250)
    (zbeacon-set-interval node3 250)
    (zbeacon-noecho node1)
    (zbeacon-publish node1 "NODE/1")
    (zbeacon-publish node2 "NODE/2")
    (zbeacon-publish node3 "GARBAGE")
    (zbeacon-subscribe node1 "NODE")

    ;;  Poll on API pipe and on UDP socket
    (with-zpollset (pollitems
		    ((zbeacon-pipe node1) :zmq-pollin)
		    ((zbeacon-pipe node2) :zmq-pollin)
		    ((zbeacon-pipe node3) :zmq-pollin))
      (loop with stop-at = (+ (zclock-time) 1000)
	 while (< (zclock-time) stop-at) do
	   (let ((timeout (- stop-at (zclock-time))))
	     (when (< timeout 0)
	       (setf timeout 0))
	     ;; fudging the call to zmq_poll. We should be using zloop.
	     (when (= -1 (cffi:foreign-funcall "zmq_poll" :pointer pollitems :int 3 :long (* timeout ZMQ_POLL_MSEC) :int))
	       (loop-finish))))

      ;;  We cannot get messages on nodes 2 and 3
      (assert (not (member :zmq-pollin (zpollset-events pollitems 1))))
      (assert (not (member :zmq-pollin (zpollset-events pollitems 2))))

      ;;  If we get a message on node 1, it must be NODE/2
      (when (assert* (member :zmq-pollin (zpollset-events pollitems 0)))
	(let ((ipaddress (zstr-recv (zbeacon-pipe node1)))
	      (beacon (zstr-recv (zbeacon-pipe node1))))
	  (declare (ignore ipaddress))
	  (assert (string= beacon "NODE/2")))))

    ;;  Stop listening
    (zbeacon-unsubscribe node1)

    ;;  Stop all node broadcasts
    (zbeacon-silence node1)
    (zbeacon-silence node2)
    (zbeacon-silence node3)

    ;; Destroy the test nodes
    (zbeacon-destroy node1)
    (zbeacon-destroy node2)
    (zbeacon-destroy node3))
    ;;  @end
  (format t "OK~%")
  0)
