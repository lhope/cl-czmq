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

(in-package :cl-user)

(defpackage :cl-czmq
  (:documentation "A re-binding of the C binding for the zmq transport layer (czmq).")
  (:use :cl)
  (:nicknames :czmq)

  (:export ;; czmq
   #:+czmq-version-major+
   #:+czmq-version-minor+
   #:+czmq-version-patch+
   #:+czmq-version+
   #:zsys-errno) ;; *

  (:export ;; zctx
   #:zctx-new
   #:zctx-destroy
   #:with-zctx ;;*
   #:zctx-shadow
   #:zctx-set-iothreads
   #:zctx-set-linger
   #:zctx-set-hwm
   #:zctx-hwm
   #:zctx-underlying
   #:zctx-interrupted
   #:zctx-test)

  (:export ;; zsockopt
   #:zsocket-type
   #:zsocket-sndhwm
   #:zsocket-rcvhwm
   #:zsocket-affinity
   #:zsocket-identity
   #:zsocket-rate
   #:zsocket-recovery-ivl
   #:zsocket-sndbuf
   #:zsocket-rcvbuf
   #:zsocket-linger
   #:zsocket-reconnect-ivl
   #:zsocket-reconnect-ivl-max
   #:zsocket-backlog
   #:zsocket-maxmsgsize
   #:zsocket-multicast-hops
   #:zsocket-rcvtimeo
   #:zsocket-sndtimeo
   #:zsocket-ipv4only
   #:zsocket-rcvmore
   #:zsocket-fd
   #:zsocket-events
   #:zsocket-last-endpoint
   #:zsocket-set-sndhwm
   #:zsocket-set-rcvhwm
   #:zsocket-set-affinity
   #:zsocket-set-subscribe
   #:zsocket-set-unsubscribe
   #:zsocket-set-identity
   #:zsocket-set-rate
   #:zsocket-set-recovery-ivl
   #:zsocket-set-sndbuf
   #:zsocket-set-rcvbuf
   #:zsocket-set-linger
   #:zsocket-set-reconnect-ivl
   #:zsocket-set-reconnect-ivl-max
   #:zsocket-set-backlog
   #:zsocket-set-maxmsgsize
   #:zsocket-set-multicast-hops
   #:zsocket-set-rcvtimeo
   #:zsocket-set-sndtimeo
   #:zsocket-set-ipv4only
   #:zsocket-set-delay-attach-on-connect
   #:zsocket-set-router-mandatory
   #:zsocket-set-router-raw
   #:zsocket-set-xpub-verbose
   #:zsocket-set-hwm
   #:zsockopt-test)

  (:export ;; zsocket
   #:+zsocket-dynfrom+
   #:+zsocket-dynto+
   #:zsocket-new
   #:zsocket-destroy
   #:with-zsockets ;; *
   #:zsocket-bind
   #:zsocket-connect
   #:zsocket-disconnect
   #:zsocket-poll
   #:zsocket-type-str
   #:zsocket-proxy
   #:zsocket-test)

  (:export ;; zframe
   #:zframe-new
   #:zframe-destroy
   #:zframe-recv
   #:zframe-recv-retry ;; *
   #:zframe-recv-nowait
   #:zframe-send
   #:zframe-size
   #:zframe-data
   #:zframe-dup
   #:zframe-strhex
   #:zframe-strdup
   #:zframe-streq
   #:zframe-more
   #:zframe-eq
   #:zframe-print
   #:zframe-reset
   #:zframe-test)

  (:export ;; zstr
   #:zstr-recv
   #:zstr-recv-retry ;; *
   #:zstr-recv-nowait
   #:zstr-send
   #:zstr-sendm
   #:zstr-test)

  (:export ;; zmsg
   #:zmsg-new
   #:zmsg-destroy
   #:zmsg-recv
   #:zmsg-recv-retry ;; *
   #:zmsg-send
   #:zmsg-size
   #:zmsg-content-size
   #:zmsg-push
   #:zmsg-pop
   #:zmsg-add
   #:zmsg-pushmem
   #:zmsg-addmem
   #:zmsg-pushstr
   #:zmsg-addstr
   #:zmsg-popstr
   #:zmsg-wrap
   #:zmsg-unwrap
   #:zmsg-remove
   #:zmsg-first
   #:zmsg-next
   #:zmsg-last
   #:zmsg-encode
   #:zmsg-decode
   #:zmsg-dup
   #:zmsg-dump
   #:zmsg-test)

  (:export ;; zbeacon
   #:zbeacon-new
   #:zbeacon-destroy
   #:zbeacon-hostname
   #:zbeacon-set-interval
   #:zbeacon-noecho
   #:zbeacon-publish
   #:zbeacon-silence
   #:zbeacon-subscribe
   #:zbeacon-unsubscribe
   #:zbeacon-pipe
   #:zbeacon-test)

  (:export ;; zthread
   #:zthread-new
   #:zthread-fork
   #:zthread-test)

  (:export ;; zpollset (lisp binding only)
   #:+zmq-poll-msec+
   #:zpollset-new     ;; *
   #:zpollset-destroy ;; *
   #:with-zpollset    ;; *
   #:zpollset-events  ;; *
   #:zpollset-pollin  ;; *
   #:zpollset-pollout ;; *
   #:zpollset-pollerr ;; *
   #:zpollset-poll)   ;; *

  (:export ;; zloop
   ;; core api
   #:zloop-new
   #:zloop-destroy
   #:with-zloop
   #:zloop-poller
   #:zloop-poller-end
   #:zloop-timer
   #:zloop-timer-end
   #:zloop-set-verbose
   #:zloop-start
   #:zloop-test))
