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

(defun zmsg-new ()
  (zmsg_new))

(defun zmsg-destroy (zmsg)
  (with-foreign-box (&zmsg zmsg)
    (zmsg_destroy &zmsg)
    (as-pointer (unbox &zmsg :pointer))))

(defun zmsg-recv (socket)
  (as-pointer (zmsg_recv socket)))

;; retrying zmsg-recv
(defun zframe-recv-retry (socket)
  (loop with msg = (zmsg-recv socket)
     when (or msg (eql (zsys-errno) :eintr))
     return msg))

(defun zmsg-send (zmsg socket)
  (with-foreign-box (&zmsg zmsg)
    (values (as-rc (zmsg_send &zmsg socket))
	    (as-pointer (unbox &zmsg :pointer)))))

(defun zmsg-size (zmsg)
  (zmsg_size zmsg))

(defun zmsg-content-size (zmsg)
  (zmsg_content_size zmsg))

(defun zmsg-push (zmsg zframe)
  (as-rc (zmsg_push zmsg zframe)))

(defun zmsg-pop (zmsg)
  (as-pointer (zmsg_pop zmsg)))

(defun zmsg-add (zmsg zframe)
  (as-rc (zmsg_add zmsg zframe)))

(defun zmsg-pushmem (zmsg data)
  (with-foreign-bytes (bytes size data)
    (as-rc (zmsg_pushmem zmsg bytes size))))

(defun zmsg-addmem (zmsg data)
  (with-foreign-bytes (bytes size data)
    (as-rc (zmsg_addmem zmsg bytes size))))

(defun zmsg-pushstr (zmsg fmt &rest args)
  (as-rc (zmsg_pushstr zmsg "%s" :string (apply #'format nil fmt args))))

(defun zmsg-addstr (zmsg fmt &rest args)
  (as-rc (zmsg_addstr zmsg "%s" :string (apply #'format nil fmt args))))

(defun zmsg-popstr (zmsg)
  (with-freed-string
    (cffi:foreign-funcall "zmsg_popstr" :pointer zmsg :pointer)))

(defun zmsg-wrap (zmsg zframe)
  (zmsg_wrap zmsg zframe))

;; 2013-07-14: issue with c implementation asserts on zframe_size <= 1
(defun zmsg-unwrap (zmsg)
  (as-pointer (zmsg_unwrap zmsg)))

(defun zmsg-remove (zmsg zframe)
  (zmsg_remove zmsg zframe))

(defun zmsg-first (zmsg)
  (as-pointer (zmsg_first zmsg)))

(defun zmsg-next (zmsg)
  (as-pointer (zmsg_next zmsg)))

(defun zmsg-last (zmsg)
  (as-pointer (zmsg_last zmsg)))

;; TODO implement lispily.
;;//  Save message to an open file, return 0 if OK, else -1.
;;CZMQ_EXPORT int
;;    zmsg_save (zmsg_t *self, FILE *file);

;; TODO implement lispily
;;//  Load/append an open file into message, create new message if
;;//  null message provided.
;;CZMQ_EXPORT zmsg_t *
;;    zmsg_load (zmsg_t *self, FILE *file);

(defun zmsg-encode (zmsg)
  (cffi:with-foreign-object (uchar** :pointer)
    ;; nullify for unwind-protect safety.
    (setf (cffi:mem-aref uchar** :pointer) (cffi:null-pointer))
    (unwind-protect
	 (loop
	    with size = (zmsg_encode zmsg uchar**) ;; allocates uchar*
	    with uchar* = (cffi:mem-aref uchar** :pointer)
	    with bytes  = (make-array size :element-type 'unsigned-byte)
	    for i below size
	    do (setf (aref bytes i) (cffi:mem-aref uchar* :uchar i))
	    finally (return bytes))
      (let ((uchar* (cffi:mem-aref uchar** :pointer)))
	(unless (cffi:null-pointer-p uchar*)
	  (cffi:foreign-free uchar*)))))) ;; the with-foreign-object frees uchar**

(defun zmsg-decode (bytes)
  (with-foreign-bytes (buffer size bytes)
    (as-pointer (zmsg_decode buffer size))))

(defun zmsg-dup (zmsg)
  (as-pointer (zmsg_dup zmsg)))

;; //  Print message to FILE stream, for debugging
;; CZMQ_EXPORT void
;;     zmsg_dump_to_stream (zmsg_t *self, FILE *file);

;; Print message to stderr, for debugging
(defun zmsg-dump (zmsg)
  (zmsg_dump zmsg))

;; //  Push block of memory as new frame to end of message.
;; //  The frame is constructed using zero-copy.
;; //  Returns 0 on success, -1 on error.
;; //  DEPRECATED - will be removed for next stable release
;; CZMQ_EXPORT int
;;     zmsg_addmem_zero_copy (zmsg_t *self, void *src, size_t size, zframe_free_fn *free_fn, void *arg);

;;  --------------------------------------------------------------------------
;;  Selftest

(defun zmsg-test (verbose)
  (format t " * zmsg: ")

  ;;  @selftest
  (let* ((ctx (assert* (zctx-new)))
	 (output (assert* (zsocket-new ctx :zmq-pair)))
	 (input (assert* (zsocket-new ctx :zmq-pair))))
    (zsocket-bind output "inproc://zmsg.test")
    (zsocket_connect input "inproc://zmsg.test")

    ;;  Test send and receive of single-frame message
    (let* ((msg (assert* (zmsg-new)))
	   (frame (assert* (zframe-new "Hello"))))
      (zmsg-push msg frame)
      (assert (= (zmsg-size msg) 1))
      (assert (= (zmsg-content-size msg) 5))
      (multiple-value-bind (rc msg)
	  (zmsg-send msg output)
	(assert (null msg))
	(assert (zerop rc))))

    (let ((msg (assert* (zmsg-recv input))))
      (assert (= (zmsg-size msg) 1))
      (assert (= (zmsg-content-size msg) 5))
      (zmsg-destroy msg))

    ;;  Test send and receive of multi-frame message
    (let ((msg (assert* (zmsg-new))))
      (assert (zerop (zmsg-addmem msg "Frame0")))
      (assert (zerop (zmsg-addmem msg "Frame1")))
      (assert (zerop (zmsg-addmem msg "Frame2")))
      (assert (zerop (zmsg-addmem msg "Frame3")))
      (assert (zerop (zmsg-addmem msg "Frame4")))
      (assert (zerop (zmsg-addmem msg "Frame5")))
      (assert (zerop (zmsg-addmem msg "Frame6")))
      (assert (zerop (zmsg-addmem msg "Frame7")))
      (assert (zerop (zmsg-addmem msg "Frame8")))
      (assert (zerop (zmsg-addmem msg "Frame9")))
      (let ((copy (assert* (zmsg-dup msg))))
	(assert (zerop (zmsg-send copy output))))
      (assert (zerop (zmsg-send msg output))))

    (let ((copy (assert* (zmsg-recv input))))
      (assert (= (zmsg-size copy) 10))
      (assert (= (zmsg-content-size copy) 60))
      (zmsg-destroy copy))

    (let ((msg (assert* (zmsg-recv input))))
      (assert (= (zmsg-size msg) 10))
      (assert (= (zmsg-content-size msg) 60))
      (when verbose
        (zmsg-dump msg))

    ;;  Save to a file, read back
    #||
    FILE *file = fopen ("zmsg.test", "w");
    assert (file);
    rc = zmsg_save (msg, file);
    assert (rc == 0);
    fclose (file);

    file = fopen ("zmsg.test", "r");
    rc = zmsg_save (msg, file);
    assert (rc == -1);
    fclose (file);
    zmsg_destroy (&msg);

    file = fopen ("zmsg.test", "r");
    msg = zmsg_load (NULL, file);
    assert (msg);
    fclose (file);
    remove ("zmsg.test");
    assert (zmsg_size (msg) == 10);
    assert (zmsg_content_size (msg) == 60);
    ||#

      ;;  Remove all frames except first and last
      (dotimes (frame_nbr 8)
        (zmsg-first msg)
        (let ((frame (zmsg-next msg)))
	  (zmsg-remove msg frame)
	  (zframe-destroy frame)))

      ;;  Test message frame manipulation
      (assert (= (zmsg-size msg) 2))
      (let ((frame (zmsg-last msg)))
	(assert (zframe-streq frame "Frame9")))
      (assert (= (zmsg-content-size msg) 12))
      (let ((frame (assert* (zframe-new "Address"))))
	(zmsg-wrap msg frame))
      (assert (= (zmsg-size msg) 4))
      (assert (zerop (zmsg-addstr msg "Body")))
      (assert (= (zmsg-size msg) 5))
      (let ((frame (zmsg-unwrap msg)))
	(zframe-destroy frame))
      (assert (= (zmsg_size msg) 3))
      (let ((body (zmsg-popstr msg)))
	(assert (string= body "Frame0")))
      (zmsg-destroy msg))

    ;;  Test encoding/decoding
    (let ((msg (assert* (zmsg_new))))
      (flet ((blank (size)
	       (make-array size
			   :element-type '(unsigned-byte 8)
			   :initial-element 0)))
	(assert (zerop (zmsg-addmem msg (blank 0))))
	(assert (zerop (zmsg-addmem msg (blank 1))))
	(assert (zerop (zmsg-addmem msg (blank 253))))
	(assert (zerop (zmsg-addmem msg (blank 254))))
	(assert (zerop (zmsg-addmem msg (blank 255))))
	(assert (zerop (zmsg-addmem msg (blank 256))))
	(assert (zerop (zmsg-addmem msg (blank 65535))))
	(assert (zerop (zmsg-addmem msg (blank 65536))))
	(assert (zerop (zmsg-addmem msg (blank 65537)))))
      (assert (= (zmsg-size msg) 9))

      (let ((buffer (zmsg-encode msg)))
	(zmsg-destroy msg)
	(setf msg (assert* (zmsg-decode buffer)))
	(zmsg-destroy msg)))

    ;;  Now try methods on an empty message
    (let ((msg (assert* (zmsg-new))))
      (assert (zerop (zmsg-size msg)))
      (assert (null (zmsg-first msg)))
      (assert (null (zmsg-last msg)))
      (assert (null (zmsg-next msg)))
      (assert (null (zmsg-pop msg)))
      (zmsg-destroy msg))

    (zctx-destroy ctx))
  ;;  @end
  (format t "OK~%")
  0)

