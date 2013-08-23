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

;; @header
;;  Provides a generic container implementing a fast singly-linked list. You
;;  can use this to construct multi-dimensional lists, and other structures
;;  together with other generic containers like zhash.

(in-package :cl-czmq)

;;  ---------------------------------------------------------------------
;;  Structure of our class

(defstruct %zlist
  head   ;;  First item in list, if any
  tail   ;;  Last item in list, if any
  cursor ;;  Current cursors for iteration
  size)  ;;  Number of items in list


;;  --------------------------------------------------------------------------
;;  List constructor

(defun zlist-new ()
  (make-%zlist :size 0))

;;  --------------------------------------------------------------------------
;;  List destructor

(defun zlist-destroy (zlist)
  (declare (ignore zlist))
  ;; no-op
  nil)

;;  --------------------------------------------------------------------------
;;  Return the item at the head of list. If the list is empty, returns NULL.
;;  Leaves cursor pointing at the head item, or NULL if the list is empty.

(defun zlist-first (zlist)
  (assert zlist)
  (setf (%zlist-cursor zlist)
	(%zlist-head zlist))

  (when (%zlist-cursor zlist)
    (car (%zlist-cursor zlist))))

;;  --------------------------------------------------------------------------
;;  Return the item at the tail of list. If the list is empty, returns NULL.
;;  Leaves cursor pointing at the tail item, or NULL if the list is empty.

(defun zlist-last (zlist)
  (assert zlist)
  (setf (%zlist-cursor zlist)
	(%zlist-tail zlist))

  (when (%zlist-cursor zlist)
    (car (%zlist-cursor zlist))))

;;  --------------------------------------------------------------------------
;;  Return the item at the head of list. If the list is empty, returns NULL.
;;  Leaves cursor as-is.

(defun zlist-head (zlist)
  (assert zlist)
  (car (%zlist-head zlist)))

;;  --------------------------------------------------------------------------
;;  Return the item at the tail of list. If the list is empty, returns NULL.
;;  Leaves cursor as-is.

(defun zlist-tail (zlist)
  (assert zlist)
  (car (%zlist-tail zlist)))

;;  --------------------------------------------------------------------------
;;  Return the next item. If the list is empty, returns NULL. To move to
;;  the start of the list call zlist_first (). Advances the cursor.

(defun zlist-next (zlist)
  (assert zlist)
  (setf (%zlist-cursor zlist)
	(if (%zlist-cursor zlist)
	    (cdr (%zlist-cursor zlist))
	    (%zlist-head zlist)))
  (car (%zlist-cursor zlist)))

;;  --------------------------------------------------------------------------
;;  Add item to the end of the list

(defun zlist-append (zlist item)
  (let ((node (list item)))
    (if (%zlist-tail zlist)
	(setf (cdr (%zlist-tail zlist)) node)
	(setf (%zlist-head zlist) node))

    (setf (%zlist-tail zlist) node)
    (incf (%zlist-size zlist))
    (setf (%zlist-cursor zlist) nil)
    nil))

;;  --------------------------------------------------------------------------
;;  Insert item at the beginning of the list

(defun zlist-push (zlist item)
  (let ((node (list item)))
    (setf (cdr node) (%zlist-head zlist)
	  (%zlist-head zlist) node)
    (unless (%zlist-tail zlist)
      (setf (%zlist-tail zlist) node))
    (incf (%zlist-size zlist))
    (setf (%zlist-cursor zlist) nil))
  t)

;;  --------------------------------------------------------------------------
;;  Remove item from the beginning of the list, returns NULL if none

(defun zlist-pop (zlist)
  (let ((node (%zlist-head zlist))
	item)
    (when node
      (setf item (car node)
	    (%zlist-head zlist) (cdr node))
      (when (eq (%zlist-tail zlist) node)
	(setf (%zlist-tail zlist) nil))
      (decf (%zlist-size zlist))
      (setf (%zlist-cursor zlist) nil))
    item))

;;  --------------------------------------------------------------------------
;;  Remove the item from the list, if present. Safe to call on items that
;;  are not in the list. Maintains cursor.

(defun zlist-remove (zlist item &key (key #'identity) (test #'eql))
  (loop
     with prev
     with item-key = (funcall key item)
     for node on (%zlist-head zlist)
     for node-item-key = (funcall key (car node))
     until (funcall test item-key node-item-key) do
       (setf prev node)
     finally
       (when node
	 (if prev
	     (setf (cdr prev) (cdr node))
	     (setf (%zlist-head zlist) (cdr node)))

	 (unless (cdr node)
	   (setf (%zlist-tail zlist) prev))

	 (when (eq (%zlist-cursor zlist) node)
	   (setf (%zlist-cursor zlist) prev))

	 (decf (%zlist-size zlist)))))


;;  --------------------------------------------------------------------------
;;  Make copy of itself

(defun zlist-dup (zlist)
  (unless zlist
    (return-from zlist-dup nil))

  (let ((copy (zlist-new)))
    (dolist (item (%zlist-head zlist))
      (zlist-append copy item))))

;;  --------------------------------------------------------------------------
;;  Return the number of items in the list

(defun zlist-size (zlist)
  (%zlist-size zlist))

;;  --------------------------------------------------------------------------
;;  Sort list. This version defers to lisp sort and sets cursor to
;;  nil.  NOTE: the compare uses the reverse logic of the C zlisp
;;  implementation.  Lisp's sort algorithm returns t iff ele1 is less
;;  than ele2. czmq's zlist_sort returns t iff ele1 is GREATER THAN
;;  ele2.

(defun zlist-sort (zlist compare &key key)
  (setf (%zlist-head zlist)
	(sort (%zlist-head zlist) compare :key key)
	(%zlist-tail zlist) (last (%zlist-head zlist))
	(%zlist-cursor zlist) nil))

;;  --------------------------------------------------------------------------
;;  Runs selftest of class

(defun zlist-test (verbose)
  (declare (ignore verbose))
  (format t " * zlist: ");

  ;;  @selftest
  (let ((list (zlist-new)))
    (assert list)
    (assert (zerop (zlist-size list)))

    ;;  Three items we'll use as test data
    ;;  List items are void *, not particularly strings
    (let ((cheese "boursin")
	  (bread  "baguette")
	  (wine   "bordeaux"))

      (zlist-append list cheese)
      (assert (= (zlist-size list) 1))
      (zlist-append list bread)
      (assert (= (zlist-size list) 2))
      (zlist-append list wine)
      (assert (= (zlist-size list) 3))

      (assert (eq (zlist-head list) cheese))
      (assert (eq (zlist-next list) cheese))

      (assert (eq (zlist-first list) cheese))
      (assert (eq (zlist-tail list) wine))
      (assert (eq (zlist-next list) bread))

      (assert (eq (zlist-first list) cheese))
      (assert (eq (zlist-next list) bread))
      (assert (eq (zlist-next list) wine))
      (assert (eq (zlist-next list) nil))
      ;;  After we reach end of list, next wraps around
      (assert (eq (zlist-next list) cheese))
      (assert (= (zlist-size list) 3))

      (zlist-remove list wine)
      (assert (= (zlist-size list) 2))

      (assert (eq (zlist-first list) cheese))
      (zlist-remove list cheese)
      (assert (= (zlist-size list) 1))
      (assert (eq (zlist-first list) bread))

      (zlist-remove list bread)
      (assert (zerop (zlist-size list)))

      (zlist-append list cheese)
      (zlist-append list bread)
      (assert (eq (zlist-last list) bread))
      (zlist-remove list bread)
      (assert (eq (zlist-last list) cheese))
      (zlist-remove list cheese)
      (assert (null (zlist-last list)))

      (zlist-push list cheese)
      (assert (= (zlist-size list) 1))
      (assert (eq (zlist-first list) cheese))

      (zlist-push list bread)
      (assert (= (zlist-size list) 2))
      (assert (eq (zlist-first list) bread))

      (zlist-append list wine)
      (assert (= (zlist-size list) 3))
      (assert (eq (zlist-first list) bread))

      (zlist-sort list #'string<)
      (let ((item (zlist-pop list)))
	(assert (eq item bread)))
      (let ((item (zlist-pop list)))
	(assert (eq item wine)))
      (let ((item (zlist-pop list)))
	(assert (eq item cheese)))
      (assert (zerop (zlist-size list)))

      ;;  Destructor should be safe to call twice
      ;;  (especially since its a no-op)
      (zlist-destroy list)
      (zlist-destroy list)
      ;;assert (list == NULL);
      ;;//  @end
      (format t "OK~%"))))
