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

(defun bitfield-options (bitfield-name options)
  (unless (listp options)
    (setf options (list options)))
  (cffi:foreign-bitfield-value bitfield-name options))

(defmacro unbox (var &optional (type :pointer))
  `(cffi:mem-aref ,var ,type))

(defmacro with-foreign-box ((var foreign &optional (type :pointer)) &body body)
  "Binds var to a type* box of one element, and sets that element to foreign."
  (let ((g!type (gensym)))
    `(let ((,g!type ,type))
       (cffi:with-foreign-object (,var ,g!type)
	 (setf (cffi:mem-aref ,var ,g!type) ,foreign)
	 ,@body))))

(defmacro as-rc (&body body)
  "c commonly uses negative int values as error markers. Converts
negative int values returned by body to nil."
  (let ((g!rc (gensym)))
    `(let ((,g!rc (progn ,@body)))
       (unless (minusp ,g!rc) ,g!rc))))

(defmacro as-bool (&body body)
  "Convert a c boolean (0 false, otherwise true) to lisp"
  (let ((g!bool (gensym)))
    `(let ((,g!bool (progn ,@body)))
       (unless (zerop ,g!bool) ,g!bool))))

(defmacro as-pointer (&body body)
  (let ((g!pointer (gensym)))
    `(let ((,g!pointer (progn ,@body)))
       (unless (cffi:null-pointer-p ,g!pointer) ,g!pointer))))

(defmacro with-foreign-bytes ((bytes-var size-var data &optional (string-encoding cffi:*default-foreign-encoding*))
			      &body body)
  "If data is a string, uses with-foreign-string. If
a (simple-array (unsigned-byte 8), uses
with-pointer-to-vector-data. If a vector of unsigned-bytes, copies
that vector using with-foreign-object."
  (let ((g!data (gensym))
	(g!func (gensym))
	(g!var (gensym))
	(g!size (gensym)))
    `(let ((,g!data ,data)
	   (,g!func (lambda (,bytes-var ,size-var) ,@body)))
       (etypecase ,g!data
	 (string
	  (cffi:with-foreign-string ((,g!var ,g!size) ,g!data :encoding ,string-encoding :null-terminated-p nil)
	    (funcall ,g!func ,g!var ,g!size)))
	 ((simple-array (unsigned-byte 8)) ;; here we're making an assumption that the c byte is 8 bits.
	  (cffi:with-pointer-to-vector-data (,g!var ,g!data)
	    (let ((,g!size (length ,g!data)))
	      (funcall ,g!func ,g!var ,g!size))))
	 (vector
	  (let ((,g!size (length ,g!data)))
	    (cffi:with-foreign-object (,g!var :uchar ,g!size)
	      (dotimes (i ,g!size)
		(setf (cffi:mem-aref ,g!var :uchar i) (aref ,g!data i)))
	      (funcall ,g!func ,g!var ,g!size))))))))

(defmacro with-freed-string (&body body)
  (let ((g!cstring (gensym)))
    `(let ((,g!cstring (cffi:null-pointer)))
       (unwind-protect
	    (cffi:foreign-string-to-lisp
	     (setf ,g!cstring (progn ,@body)))
	 (unless (cffi:null-pointer-p ,g!cstring)
	   (cffi:foreign-free ,g!cstring)))))) ;; should work on sbcl at least

(defmacro assert* (test &rest assert-args)
  "Like assert, except if test succeeds, returns it."
  (let ((g!test (gensym)))
    `(let ((,g!test ,test))
       (assert ,g!test ,@assert-args)
       ,g!test)))
