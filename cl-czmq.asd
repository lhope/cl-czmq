;;;; -*- Mode: LISP -*-
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

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem :cl-czmq
  :name "cl-czmq"
  :version "0.1.0"
  :author "Lucas Hope <lucas.r.hope@gmail.com>"
  :license "LLGPL"
  :maintainer "Lucas Hope <lucas.r.hope@gmail.com"
  :description "A re-binding of the C binding for the zmq transport layer (czmq)."
  :depends-on (:cffi :bordeaux-threads)
  :components ((:module "src"
		:serial t
                :components ((:file "package")
                             (cffi-grovel:grovel-file "grovel")
			     (:file "ffi")
			     (:file "ffi-utils")
			     (:file "czmq")
			     (:file "zsys")
			     (:file "zctx")
			     (:file "zsocket")
			     (:file "zsockopt")
			     (:file "zframe")
			     (:file "zstr")
			     (:file "zmsg")
			     (:file "zpollset")
			     (:file "zclock")
			     (:file "zloop")
			     (:file "zbeacon")
			     (:file "zthread")
			     (:file "zlist")
			     (:file "zhash")))))
