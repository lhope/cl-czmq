%module "ffi"

%import <zmq.h>
%insert("lisphead")
%{
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

(cffi:define-foreign-library libczmq
  (t (:default "libczmq")))

(cffi:use-foreign-library libczmq)
%}

%ignore "CZMQ_ASSERT_SANE_FUNCTION";

%typemap(cout) int64_t ":int64";
%typemap(cin)  size_t "size_t";
%typemap(cout) size_t "size_t";
%typemap(cin)  ssize_t "ssize_t";
%typemap(cout) ssize_t "ssize_t";
%typemap(cin)  bool "bool";
%typemap(cout) bool "bool";
%typemap(cin)  mode_t "mode_t";
%typemap(cout) mode_t "mode_t";

%typemap(cin)  byte ":uint8";
%typemap(cout) byte ":uint8";


%include "czmq.h"
%include "czmq_prelude.h"
%include "zbeacon.h"
%include "zclock.h"
 /* %include "zconfig.h" */
%include "zctx.h"
%include "zfile.h"
%include "zframe.h"
%include "zhash.h"
%include "zlist.h"
%include "zloop.h"
%include "zmsg.h"
%include "zmutex.h"
%include "zsocket.h"
%include "zsockopt.h"
%include "zstr.h"
%include "zsys.h"
%include "zthread.h"
 /* %include "ztree.h" */
