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

(defun zframe-new (data)
  (with-foreign-bytes (bytes size data)
    ;; copies bytes.
    (zframe_new bytes size)))

#+ignore ;; not in 1.4.1
(defun zframe-new-empty ()
  (zframe_new_empty))

(defun zframe-destroy (zframe)
  "Returns the new value of zframe (should be null pointer)"
  (with-foreign-box (&zframe zframe)
    (zframe_destroy &zframe)
    (as-pointer (unbox &zframe :pointer))))

(defun %zframe-recv (socket)
  (as-pointer (zframe_recv socket)))

;; retrying zframe-recv
(defun zframe-recv (socket &optional (retry *zsys-retry*))
  (loop for frame = (%zframe-recv socket)
     when (or frame (not retry) (not (eql (zsys-errno) :eintr)))
     return frame))

(defun zframe-recv-nowait (socket)
  (as-pointer (zframe_recv_nowait socket)))

(defun zframe-send (zframe socket &rest zframe-options)
  ;; note this destroys frame.
  (with-foreign-box (&zframe zframe)
    (values (as-rc (zframe_send &zframe socket (bitfield-options 'zframe-options zframe-options)))
	    (as-pointer (unbox &zframe)))))

(defun zframe-size (zframe)
  (zframe_size zframe))

(defun zframe-data (zframe)
  (let* ((bytes (zframe_data zframe))
	 (size (zframe-size zframe))
	 (data (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size data)
      (setf (aref data i) (cffi:mem-aref bytes :uchar i)))))

(defun zframe-dup (zframe)
  (zframe_dup zframe))

(defun zframe-strhex (zframe)
  (with-freed-string
    (cffi:foreign-funcall "zframe_strhex" :pointer zframe :pointer)))

(defun zframe-strdup (zframe)
  ;; done this way to free the foreign string.
  (with-freed-string
    (cffi:foreign-funcall "zframe_strdup" :pointer zframe :pointer)))

(defun zframe-streq (zframe string)
  (as-bool (zframe_streq zframe string)))

(defun zframe-more (zframe)
  (as-bool (zframe_more zframe)))

#+ignore ;; not in 1.4.1
(defun zframe-set-more (zframe more)
  (zframe_set_more zframe (if more 1 0)))

(defun zframe-eq (zframe-1 zframe-2)
  (as-bool (zframe_eq (or zframe-1 (cffi:null-pointer))
		      (or zframe-2 (cffi:null-pointer)))))

;; no easy way to convert lisp streams to FILE *.
;;//   Print contents of the frame to FILE stream.
;;CZMQ_EXPORT void
;;    zframe_print_to_stream (zframe_t *self, const char *prefix, FILE *file);

(defun zframe-print (zframe prefix)
  (zframe_print zframe prefix))

(defun zframe-reset (zframe data)
  (with-foreign-bytes (bytes size data)
    ;; copies bytes.
    (zframe_reset zframe bytes size)))

;; //  Callback function for zframe_free_fn method
;; //  DEPRECATED - will be removed for next stable release
;; typedef void (zframe_free_fn) (void *data, void *arg);

;; //  Create a zero-copy frame
;; //  DEPRECATED - will be removed for next stable release
;; CZMQ_EXPORT zframe_t *
;;     zframe_new_zero_copy (void *data, size_t size,
;;                           zframe_free_fn *free_fn, void *arg);

;; // Return frame zero copy indicator (1 or 0)
;; //  DEPRECATED - will be removed for next stable release
;; CZMQ_EXPORT int
;;     zframe_zero_copy (zframe_t *self);

;; //  Set the free callback for frame
;; //  DEPRECATED - will be removed at next stable release
;; CZMQ_EXPORT void
;;     zframe_freefn (zframe_t *self, zframe_free_fn *free_fn, void *arg);

;;  --------------------------------------------------------------------------
;;  Selftest

(defun zframe-test (verbose)
  (declare (ignore verbose))
  (format t " * zframe: ")

  ;;  @selftest
  (let* ((ctx (assert* (zctx-new)))
	 (output (assert* (zsocket-new ctx :zmq-pair)))
	 (input (assert* (zsocket-new ctx :zmq-pair))))
    (zsocket-bind output "inproc://zframe.test")
    (zsocket-connect input "inproc://zframe.test")
    ;;  Send five different frames, test ZFRAME_MORE
    (dotimes (frame-nbr 5)
      (let ((frame (zframe-new "Hello")))
        (assert (zerop (zframe-send frame output :zframe-more)))))

    ;;  Send same frame five times, test ZFRAME_REUSE
    (let ((frame (assert* (zframe-new "Hello"))))
      (dotimes (frame-nbr 5)
        (assert (zerop (zframe-send frame output :zframe-more :zframe-reuse))))

      (assert frame)
      (let ((copy (zframe-dup frame)))
	(assert (zframe-eq frame copy))
	(setf frame (zframe-destroy frame))
	(assert (not (zframe-eq frame copy)))
	(assert (= (zframe-size copy) 5))
	(zframe-destroy copy)
	(assert (not (zframe-eq frame copy)))))

    ;;  Send END frame
    (let ((frame (assert* (zframe-new "NOT"))))
      (zframe-reset frame "END")
      (let ((string (zframe-strhex frame)))
	(assert (string= string "454E44")))
      (let ((string (zframe-strdup frame)))
	(assert (string= string "END")))
      (assert (zerop (zframe-send frame output))))

    ;;  Read and count until we receive END
    (loop for frame-nbr from 0
       for frame = (zframe-recv input) do
	 (when (zframe-streq frame "END")
	   (zframe-destroy frame)
	   (loop-finish))
	 (assert (zframe-more frame))
	 (zframe-destroy frame)
       finally
	 (assert (= frame-nbr 10))
	 (assert (null (zframe-recv-nowait input))))

    #|| not relevant tests
    // Test zero copy
    char *buffer = (char *) malloc (1024);
    int i;
    for (i = 0; i < 1024; i++)
        buffer [i] = 'A';

    frame = zframe_new_zero_copy (buffer, 1024, s_test_free_cb, NULL);
    zframe_t *frame_copy = zframe_dup (frame);

    assert (zframe_zero_copy (frame) == 1);
    assert (zframe_zero_copy (frame_copy) == 0);

    zframe_destroy (&frame);
    zframe_destroy (&frame_copy);

    frame = zframe_new ("callback", 8);
    zframe-freefn (frame, s_test_free_frame_cb, NULL);
    zframe_destroy (&frame);
    ||#

    (zctx-destroy ctx))
  ;;  @end
  (format t "OK~%")
  0)
