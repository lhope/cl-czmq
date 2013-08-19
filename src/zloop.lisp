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

;; This is a direct port of czmq/src/zloop.c which is
;;  Copyright (c) 1991-2013 iMatix Corporation <www.imatix.com>
;;  Copyright other contributors as noted in the AUTHORS file.

;;  The zloop class provides an event-driven reactor pattern. The reactor
;;  handles zmq_pollitem_t items (pollers or writers, sockets or fds), and
;;  once-off or repeated timers. Its resolution is 1 msec. It uses a tickless
;;  timer to reduce CPU interrupts in inactive processes.

(in-package :cl-czmq)

;;  Structure of our class
(defstruct %zloop
  pollers   ;;  List of poll items
  timers    ;;  List of timers
  poll-size ;;  Size of poll set
  pollset   ;;  zmq_poll set
  pollact   ;;  Lisp array of oollers for this poll set
  dirty     ;;  True if pollset needs rebuilding
  verbose   ;;  True if verbose tracing wanted
  zombies   ;;  List of timers to kill
  )

;;  Pollers and timers are held as small structures of their own
(defstruct %poller
  item          ;; zmq_pollitem_t
  handler       ;; zloop_fn
  args          ;; void *arg - a list of args to apply with the handler
  ignore-errors ;; bool
  errors        ;; int -  If too many errors, kill poller
  )

(defstruct %timer
  delay   ;; size_t
  times   ;; size_t
  handler ;; zloop_fn
  args    ;; void *arg - a list of args to apply with the handler
  when    ;; int64_t when - Clock time when alarm goes off
  )

(defun %zloop-log (format &rest args)
  (flet ((iso-8601-time ()
	   (multiple-value-bind (second minute hour date month year)
	       (decode-universal-time (get-universal-time) 0)
	     (let ((msec (mod (get-internal-real-time) 1000)))
	     (format nil "~D-~2,'0D-~2,'0D ~D:~2,'0D:~2,'0D.~3,'0D"
		     year month date hour minute second msec)))))
    (format *error-output* "[~A] - " (iso-8601-time)))
  (apply #'format *error-output* format args)
  (fresh-line *error-output*))

(defun %item-copy (from-ptr &optional to-ptr)
  (assert (cffi:pointerp from-ptr))
  (unless to-ptr
    (setf to-ptr (cffi:foreign-alloc '(:struct zpoller))))
  (assert (cffi:pointerp from-ptr))
  (cffi:foreign-funcall "memcpy" :pointer to-ptr :pointer from-ptr
			size_t (cffi:foreign-type-size '(:struct zpoller))
			:pointer)
  to-ptr)

(defun %poller-new (item handler args)
  (cffi:with-foreign-slots ((events) item (:struct zpoller))
    (make-%poller
     :item (%item-copy item)
     :handler handler
     :args args
     :ignore-errors
     (and
      (member :zmq-ignrerr (cffi:foreign-bitfield-symbols 'event-types events))
      t))))

(defun %timer-new (delay times handler args)
  (make-%timer
   :delay delay
   :times times
   :handler handler
   :args args
   :when -1)) ;;  Indicates a new timer

(defun %rebuild-pollset (zloop)
  "We hold an array of pollers that matches the pollset, so we can
register/cancel pollers orthogonally to executing the pollset
activity on pollers. Returns t on success, nil on failure."
  (when (%zloop-pollset zloop)
    (cffi:foreign-free (%zloop-pollset zloop)))

  (setf (%zloop-poll-size zloop)
	(length (%zloop-pollers zloop)))

  ;; copy the poll list as a vector
  (setf (%zloop-pollact zloop)
	(coerce (%zloop-pollers zloop) 'vector))

  ;; make the pollset which should mirror the items in the pollers. Copy by value.
  (setf (%zloop-pollset zloop)
	(loop with item-size = (cffi:foreign-type-size '(:struct zpoller))
	   with pollset =
	     (cffi:foreign-alloc '(:struct zpoller) :count (%zloop-poll-size zloop))
	   for item-nbr from 0
	   for poller across (%zloop-pollact zloop)
	   for item = (cffi:inc-pointer pollset (* item-size item-nbr)) do
	     (%item-copy (%poller-item poller) item)
	   finally
	     (return pollset)))

  (setf (%zloop-dirty zloop) nil)
  t)

(defun s-tickless-timer (zloop)
  ;;  Calculate tickless timer, up to 1 hour
  (let ((tickless (+ (get-internal-real-time)
		     (* 1000 3600))))
    (dolist (timer (%zloop-timers zloop))
      (when (= -1 (%timer-when timer))
	(setf (%timer-when timer)
	      (+ (%timer-delay timer)
		 (get-internal-real-time))))
      (setf tickless (min tickless (%timer-when timer))))
    (let ((timeout (max 0 (- tickless (get-internal-real-time)))))
      (when (%zloop-verbose zloop)
	(%zloop-log "I: zloop: polling for ~D msec" timeout))
      timeout)))

(defun %ptr-addr (ptr)
  (format nil "~8,'0x" (cffi:pointer-address ptr)))

;;  --------------------------------------------------------------------------
;;  Constructor

(defun zloop-new ()
  (make-%zloop))


;;  --------------------------------------------------------------------------
;;  Destructor

(defun zloop-destroy (self)
  (assert self)

  (when (%zloop-pollset self)
    (cffi:foreign-free (%zloop-pollset self)))
  (dolist (poller (%zloop-pollers self))
    (let ((item (%poller-item poller)))
      (cffi:foreign-free item)))
  nil)


(defmacro with-zloop ((zloop) &body body)
  `(let ((,zloop (zloop-new)))
     (unwind-protect
	  (progn ,@body)
       (zloop-destroy ,zloop))))

;;  --------------------------------------------------------------------------
;;  Register pollitem with the reactor. When the pollitem is ready, will call
;;  the handler, passing the arg. Returns 0 if OK, -1 if there was an error.
;;  If you register the pollitem more than once, each instance will invoke its
;;  corresponding handler.

(defun zloop-poller (self item handler &rest args)
  (cffi:with-foreign-slots
      ((socket fd) item (:struct zpoller))
    (when (and (cffi:null-pointer-p socket)
	       (zerop fd))
      (return-from zloop-poller))

    (unless (cffi:null-pointer-p socket)
      (when (string= (zsocket-type-str socket) "UNKNOWN")
	(return-from zloop-poller)))

    (let ((poller (%poller-new item handler args)))
      (when poller
	(push poller (%zloop-pollers self))
	(setf (%zloop-dirty self) t)
	(when (%zloop-verbose self)
	  (%zloop-log "I: zloop: register ~A poller (~A, ~d)"
		      (if (cffi:null-pointer-p socket)
			  "FD" (zsocket-type-str socket))
		      (%ptr-addr socket) fd))
	t))))


;;  --------------------------------------------------------------------------
;;  Cancel a pollitem from the reactor, specified by socket or FD. If both
;;  are specified, uses only socket. If multiple poll items exist for same
;;  socket/FD, cancels ALL of them.


(defun zloop-poller-end (self item)
  (assert self)
  (cffi:with-foreign-slots ((socket fd) item (:struct zpoller))
    (assert (or (not (cffi:null-pointer-p socket))
		(not (zerop fd))))
    (let ((item->socket socket)
	  (item->fd fd))
      (flet ((poller-match-p (poller)
	       (cffi:with-foreign-slots ((socket fd) (%poller-item poller) (:struct zpoller))
		 (when (or (and (not (cffi:null-pointer-p item->socket))
				(cffi:pointer-eq item->socket socket))
			   (and (not (zerop item->fd))
				(= item->fd fd)))
		   (cffi:foreign-free (%poller-item poller))
		   (setf (%zloop-dirty self) t)
		   t))))
	(setf (%zloop-pollers self)
	      (delete-if #'poller-match-p (%zloop-pollers self)))))
    (when (%zloop-verbose self)
      (%zloop-log "I: zloop: cancel ~s poller (~a, ~d)"
		  (if (cffi:null-pointer-p socket)
		      "FD" (zsocket-type-str socket))
		  (%ptr-addr socket) fd))))


;;  --------------------------------------------------------------------------
;;  Register a timer that expires after some delay and repeats some number of
;;  times. At each expiry, will call the handler, passing the arg. To
;;  run a timer forever, use 0 times. Returns t if OK, nil if there was an
;;  error.

(defun zloop-timer (self delay times handler &rest args)
  (assert self)
  (let ((timer (%timer-new delay times handler args)))
    (unless timer (return-from zloop-timer))

    (push timer (%zloop-timers self))

    (when (%zloop-verbose self)
      (%zloop-log "I: zloop: register timer delay=~d times=~d" delay times))
    t))


;;  --------------------------------------------------------------------------
;;  Cancel all timers for a specific argument (as provided in zloop_timer)
;;  Returns 0 on success.

(defun zloop-timer-end (self &rest args)
  (assert self)
  ;;(assert args) ;; really?

  ;;  We cannot touch self->timers because we may be executing that
  ;;  from inside the poll loop. So, we hold the arg on the zombie
  ;;  list, and process that list when we're done executing timers.
  (push args (%zloop-zombies self))

  (when (%zloop-verbose self)
    (%zloop-log "I: zloop: cancel timer")))

;;  --------------------------------------------------------------------------
;;  Set verbose tracing of reactor on/off
(defun zloop-set-verbose (self verbose)
  (setf (%zloop-verbose self) verbose))


;;  --------------------------------------------------------------------------
;;  Start the reactor. Takes control of the thread and returns when the 0MQ
;;  context is terminated or the process is interrupted, or any event handler
;;  returns nil. Event handlers may register new sockets and timers, and
;;  cancel sockets. Returns 0 if interrupted, nil if canceled by a
;;  handler, positive on internal error

(defun %zmq-err ()
  (let ((errno (cffi:foreign-funcall "zmq_errno" :int)))
    (format nil "~a (~D=~a)"
	    (cffi:foreign-funcall "zmq_strerror" :int errno :string)
	    errno (cffi:foreign-enum-keyword 'error-code errno))))

(defun zloop-start (self)
  (assert self)

  ;;  Recalculate all timers now
  (dolist (timer (%zloop-timers self))
    (setf (%timer-when timer)
	  (+ (get-internal-real-time) (%timer-delay timer))))


  ;;  Main reactor loop
  (loop with rc = 0
     until (zctx-interrupted) do
       (when (%zloop-dirty self)
	 ;; If s_rebuild_pollset() fails, break out of the loop and
	 ;; return its error
	 (unless (%rebuild-pollset self)
	   (setf rc nil)
	   (loop-finish)))

       (when (or (null (zpollset-poll (%zloop-pollset self)
				      (%zloop-poll-size self)
				      (* (s-tickless-timer self) zmq_poll_msec)))
		 (zctx-interrupted))
	 (when (%zloop-verbose self)
	   (%zloop-log "I: zloop: interrupted - ~s"
		       (%zmq-err)))
	 (setf rc 0)
	 (loop-finish)) ;; Context has been shut down.

     ;;  Handle any timers that have now expired
       (loop with to-remove
	  for timer in (%zloop-timers self)
	  for when = (%timer-when timer) do
	    (when (and (/= -1 when)
		       (>= (get-internal-real-time) when))
	      (when (%zloop-verbose self)
		(%zloop-log "I: zloop: call timer handler"))

	      (unless (setf rc (apply (%timer-handler timer) self nil (%timer-args timer)))
		(loop-finish)) ;;  Timer handler signaled break

	      (if (and (plusp (%timer-times timer))
		       (zerop (decf (%timer-times timer))))
		  (push timer to-remove)
		  (setf (%timer-when timer)
			(+ (%timer-delay timer) (get-internal-real-time)))))
	  finally
	    (setf (%zloop-timers self)
		  (nset-difference (%zloop-timers self) to-remove)))

     ;;  Handle any pollers that are ready
       (loop with item-size = (cffi:foreign-type-size '(:struct zpoller))
	  for item-nbr below (%zloop-poll-size self)
	  for poller = (aref (%zloop-pollact self) item-nbr)
	  for item = (cffi:inc-pointer (%zloop-pollset self) (* item-size item-nbr)) do
	    (cffi:with-foreign-slots ((socket fd revents) item (:struct zpoller))
	      (assert (cffi:pointer-eq
		       socket
		       (cffi:foreign-slot-value (%poller-item poller)
						'(:struct zpoller) 'socket)))

	      (cond ((and (member :zmq-pollerr (zpollset-events item))
			  (not (%poller-ignore-errors poller)))
		     (when (%zloop-verbose self)
		       (%zloop-log "I: zloop: can't poll %a socket (~A, ~d): ~A"
				   (if (cffi:null-pointer-p socket)
				       "FD" (zsocket-type-str socket))
				   (%ptr-addr socket) fd
				   (%zmq-err)))
		     ;;  Give handler one chance to handle error, then kill
		     ;;  poller because it'll disrupt the reactor otherwise.
		     (unless (zerop (%poller-errors poller))
		       (zloop-poller-end self (%poller-item poller))
		       (setf revents 0))
		     (incf (%poller-errors poller)))
		    (t
		     (setf (%poller-errors poller) 0) ;;  A non-error happened
		     ))

	      (unless (zerop revents)
                (when (%zloop-verbose self)
		  (%zloop-log "I: zloop: call ~a socket handler (~A, ~d)"
			      (if (cffi:null-pointer-p socket)
				  "FD" (zsocket-type-str socket))
			      (%ptr-addr socket) fd))

                (setf rc (apply (%poller-handler poller)
				self item (%poller-args poller)))
                (unless rc
		  (loop-finish)) ;;  Poller handler signaled break
                ;; If the poller handler calls zloop_poller_end on poller other than itself
                ;; we need to force rebuild in order to avoid reading from freed memory in the handler
                (when (%zloop-dirty self)
		  (when (%zloop-verbose self)
		    (%zloop-log "I: zloop: pollers canceled, forcing rebuild"))
		  (loop-finish)))))

     ;;  Now handle any timer zombies
     ;;  This is going to be slow if we have many zombies
       (flet ((args-equal (args1 args2)
		(and (= (length args1) (length args2))
		     (loop
			for arg1 in args2
			for arg2 in args2
			unless (or (equal arg1 arg2)
				   (and (cffi:pointerp arg1)
					(cffi:pointerp arg2)
					(cffi:pointer-eq arg1 arg2)))
			return nil
			finally (return t)))))

	 (dolist (zombie-args (%zloop-zombies self))
	   (setf (%zloop-timers self)
		 (delete-if (lambda (timer)
			      (args-equal zombie-args (%timer-args timer)))
			    (%zloop-timers self)))))

       (unless rc
	 (loop-finish))
     finally
       (return rc)))


;;  --------------------------------------------------------------------------
;;  Selftest

(defun s-timer-event (loop item output)
  (declare (ignore loop item))
  (zstr-send output "PING")
  t)


(defun s-socket-event (loop item)
  (declare (ignore loop item))
  nil)

(defun zloop-test (verbose)
  (format t " * zloop: ")

  (with-zctx (ctx)
    (assert ctx)

    (with-zsockets ctx
	((output :zmq-pair)
	 (input :zmq-pair))
      (assert output)
      (zsocket-bind output "inproc://zloop.test")
      (assert input)
      (zsocket-connect input "inproc://zloop.test")

      (with-zloop (loop)
	(assert loop)
	(zloop-set-verbose loop verbose)

	;;  After 10 msecs, send a ping message to output
	(zloop-timer loop 10 1 #'s-timer-event output)

	;;  When we get the ping message, end the reactor
	(with-zpollset (poll-input (input :zmq-pollin))
	  (assert (zloop-poller loop poll-input #'s-socket-event)))

	(zloop-start loop))))

  ;;  @end
  (format t "OK~%"))
