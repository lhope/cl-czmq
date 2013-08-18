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

(include "zmq.h")
(include "czmq.h")

(in-package :cl-czmq)

;; czmq defs.
(ctype size_t "size_t")
(ctype ssize_t "ssize_t")
(ctype bool   "bool")
(ctype mode_t "mode_t")

(bitfield zframe-options
	  ((:zframe-more "ZFRAME_MORE"))
	  ((:zframe-reuse "ZFRAME_REUSE"))
	  ((:zframe-dontwait "ZFRAME_DONTWAIT")))


;; zmq defs.
;; need pollitem, constants from zmq.h (?)

(constantenum error-code
              ;; Standard error codes
              ((:einval "EINVAL"))
              ((:enodev "ENODEV"))
              ((:eintr "EINTR"))
              ((:efault "EFAULT"))
              ((:enomem "ENOMEM"))
              ((:eagain "EAGAIN"))
              ((:emfile "EMFILE"))
              ((:enotsup "ENOTSUP"))
              ((:eprotonosupport "EPROTONOSUPPORT"))
              ((:enobufs "ENOBUFS"))
              ((:enetdown "ENETDOWN"))
              ((:eaddrinuse "EADDRINUSE"))
              ((:eaddrnotavail "EADDRNOTAVAIL"))
              ((:econnrefused "ECONNREFUSED"))
              ((:einprogress "EINPROGRESS"))
              ((:enotsock "ENOTSOCK"))
              ;; ZMQ native error codes
              ((:efsm "EFSM"))
              ((:enocompatproto "ENOCOMPATPROTO"))
              ((:eterm "ETERM"))
              ((:emthread "EMTHREAD")))

(bitfield event-types
          ((:zmq-pollin "ZMQ_POLLIN"))
          ((:zmq-pollout "ZMQ_POLLOUT"))
          ((:zmq-pollerr "ZMQ_POLLERR"))
	  ((:zmq-ignerr "ZMQ_IGNERR")))

(constantenum socket-type
              ((:zmq-pair "ZMQ_PAIR"))
              ((:zmq-pub "ZMQ_PUB"))
              ((:zmq-sub "ZMQ_SUB"))
              ((:zmq-req "ZMQ_REQ"))
              ((:zmq-rep "ZMQ_REP"))
              ((:zmq-dealer "ZMQ_DEALER"))
              ((:zmq-router "ZMQ_ROUTER"))
              ((:zmq-pull "ZMQ_PULL"))
              ((:zmq-push "ZMQ_PUSH"))
              ((:zmq-xpub "ZMQ_XPUB"))
              ((:zmq-xsub "ZMQ_XSUB"))
              ((:zmq-xreq "ZMQ_XREQ"))
              ((:zmq-xrep "ZMQ_XREP"))
              ((:zmq-upstream "ZMQ_UPSTREAM"))
              ((:zmq-downstream "ZMQ_DOWNSTREAM")))
