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

(defmacro zsocket-getter (type)
  (let ((c-name (intern (format nil "~:@(zsocket_~A~)"
				(substitute #\_ #\- (string type)))))
	(l-name (intern (format nil "~:@(zsocket-~A~)"
				(substitute #\- #\_ (string type))))))
    `(defun ,l-name (zocket) (,c-name zocket))))

(zsocket-getter type)
(zsocket-getter sndhwm)
(zsocket-getter rcvhwm)
(zsocket-getter affinity)

;;  Returns freshly allocated string, free when done
(defun zsocket-identity (zocket)
  (with-freed-string
    (cffi:foreign-funcall "zsocket_identity" :pointer zocket :pointer)))

(zsocket-getter rate)
(zsocket-getter recovery-ivl)
(zsocket-getter sndbuf)
(zsocket-getter rcvbuf)
(zsocket-getter linger)
(zsocket-getter reconnect-ivl)
(zsocket-getter reconnect-ivl-max)
(zsocket-getter backlog)
(zsocket-getter maxmsgsize)
(zsocket-getter multicast-hops)
(zsocket-getter rcvtimeo)
(zsocket-getter sndtimeo)
(zsocket-getter ipv4only)
(zsocket-getter rcvmore)
(zsocket-getter fd)
(zsocket-getter events)

;;  Returns freshly allocated string, free when done
(defun zsocket-last-endpoint (zocket)
  (with-freed-string
    (cffi:foreign-funcall "zsocket_last_endpoint" :pointer zocket :pointer)))

;;  Set socket options
(defmacro zsocket-setter (type)
  (let ((c-name (intern (format nil "~:@(zsocket_set_~A~)"
				(substitute #\_ #\- (string type)))))
	(l-name (intern (format nil "~:@(zsocket-set-~A~)"
				(substitute #\- #\_ (string type))))))
    `(defun ,l-name (zocket ,type) (,c-name zocket ,type) (values))))

(zsocket-setter sndhwm)
(zsocket-setter rcvhwm)
(zsocket-setter affinity)

;; unlike the rest, these take char* input. Should be fine...
(zsocket-setter subscribe)
(zsocket-setter unsubscribe)
(zsocket-setter identity)

(zsocket-setter rate)
(zsocket-setter recovery-ivl)
(zsocket-setter sndbuf)
(zsocket-setter rcvbuf)
(zsocket-setter linger)
(zsocket-setter reconnect-ivl)
(zsocket-setter reconnect-ivl-max)
(zsocket-setter backlog)
(zsocket-setter maxmsgsize)
(zsocket-setter multicast-hops)
(zsocket-setter rcvtimeo)
(zsocket-setter sndtimeo)
(zsocket-setter ipv4only)
(zsocket-setter delay-attach-on-connect)
(zsocket-setter router-mandatory)
(zsocket-setter router-raw)
(zsocket-setter xpub-verbose)

;;  Emulation of widely-used 2.x socket options
(zsocket-setter hwm)

;;  --------------------------------------------------------------------------
;;  Selftest

(defun zsockopt-test (verbose)
  (declare (ignore verbose))
  (format t " * zsockopt: ")

  ;;  @selftest
  (let ((ctx (assert* (zctx-new))))
    ;;#if (ZMQ_VERSION_MAJOR == 3)
    (macrolet ((with-zocket (type &body body)
		 `(let ((zocket (assert* (zsocket-new ctx ,type))))
		    ,@body
		    (zsocket-destroy ctx zocket))))
      (with-zocket :zmq-sub
	(zsocket-type zocket))
      (with-zocket :zmq-pub
	(zsocket-set-sndhwm zocket 1)
	(assert (= (zsocket-sndhwm zocket) 1))
	(zsocket-sndhwm zocket))
      (with-zocket :zmq-sub
	(zsocket-set-rcvhwm zocket 1)
	(assert (= (zsocket-rcvhwm zocket) 1))
	(zsocket-rcvhwm zocket))
      (with-zocket :zmq-sub
	(zsocket-set-affinity zocket 1)
	(assert (= (zsocket-affinity zocket) 1))
	(zsocket-affinity zocket))
      (with-zocket :zmq-sub
	(zsocket-set-subscribe zocket "test"))
      (with-zocket :zmq-sub
	(zsocket-set-unsubscribe zocket "test"))
      (with-zocket :zmq-dealer
	(zsocket-set-identity zocket "test")
	(assert (zsocket-identity zocket)))
      (with-zocket :zmq-sub
	(zsocket-set-rate zocket 1)
	(assert (= (zsocket-rate zocket) 1))
	(zsocket-rate zocket))
      (with-zocket :zmq-sub
	(zsocket-set-recovery-ivl zocket 1)
	(assert (= (zsocket-recovery-ivl zocket) 1))
	(zsocket-recovery-ivl zocket))
      (with-zocket :zmq-pub
	(zsocket-set-sndbuf zocket 1)
	(assert (= (zsocket-sndbuf zocket) 1))
	(zsocket-sndbuf zocket))
      (with-zocket :zmq-sub
	(zsocket-set-rcvbuf zocket 1)
	(assert (= (zsocket-rcvbuf zocket) 1))
	(zsocket-rcvbuf zocket))
      (with-zocket :zmq-sub
	(zsocket-set-linger zocket 1)
	(assert (= (zsocket-linger zocket) 1))
	(zsocket-linger zocket))
      (with-zocket :zmq-sub
	(zsocket-set-reconnect-ivl zocket 1)
	(assert (= (zsocket-reconnect-ivl zocket) 1))
	(zsocket-reconnect-ivl zocket))
      (with-zocket :zmq-sub
	(zsocket-set-reconnect-ivl-max zocket 1)
	(assert (= (zsocket-reconnect-ivl-max zocket) 1))
	(zsocket-reconnect-ivl-max zocket))
      (with-zocket :zmq-sub
	(zsocket-set-backlog zocket 1)
	(assert (= (zsocket-backlog zocket) 1))
	(zsocket-backlog zocket))
      (with-zocket :zmq-sub
	(zsocket-set-maxmsgsize zocket 1)
	(assert (= (zsocket-maxmsgsize zocket) 1))
	(zsocket-maxmsgsize zocket))
      (with-zocket :zmq-sub
	(zsocket-set-multicast-hops zocket 1)
	(assert (= (zsocket-multicast-hops zocket) 1))
	(zsocket-multicast-hops zocket))
      (with-zocket :zmq-sub
	(zsocket-set-rcvtimeo zocket 1)
	(assert (= (zsocket-rcvtimeo zocket) 1))
	(zsocket-rcvtimeo zocket))
      (with-zocket :zmq-sub
	(zsocket-set-sndtimeo zocket 1)
	(assert (= (zsocket-sndtimeo zocket) 1))
	(zsocket-sndtimeo zocket))
      (with-zocket :zmq-sub
	(zsocket-set-ipv4only zocket 1)
	(assert (= (zsocket-ipv4only zocket) 1))
	(zsocket-ipv4only zocket))
      (with-zocket :zmq-pub
	(zsocket-set-delay-attach-on-connect zocket 1))
      (with-zocket :zmq-router
	(zsocket-set-router-mandatory zocket 1))
      (with-zocket :zmq-router
	(zsocket-set-router-raw zocket 1))
      (with-zocket :zmq-xpub
	(zsocket-set-xpub-verbose zocket 1))
      (with-zocket :zmq-sub
	(zsocket-rcvmore zocket))
      (with-zocket :zmq-sub
	(zsocket-fd zocket))
      (with-zocket :zmq-sub
	(zsocket-events zocket))
      (with-zocket :zmq-sub
	(assert (zsocket-last-endpoint zocket)))
      (with-zocket :zmq-sub
	(zsocket-set-hwm zocket 1)))

    (zctx-destroy ctx))
  ;;  @end

  (format t "OK~%")
  0)

