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
;;   Expandable hash table container
;; @discuss
;;   Implements the zhash api, but uses a lisp 'equal hash table as its backend.
;;   Uses a special nil hash key to store an equal hash of free functions.
;;   It doesn't support autofree as garbage collection is appropriate in that case.
;; @end


(in-package :cl-czmq)

;;  Hash table performance parameters

(defconstant +initial-size+ 255)  ;;  Initial size in items
(defconstant +load-factor+ 75)    ;;  Percent loading before splitting
(defconstant +growth-factor+ 200) ;;  Increase in % after splitting


;;  --------------------------------------------------------------------------
;;  Local helper function
;;  Lookup item in hash table, returns item or NULL

;;static item_t *
;;s_item_lookup (zhash_t *self, const char *key)
;; => (gethash key zhash)

;;  --------------------------------------------------------------------------
;;  Local helper functions for managing free-fns

(defun %zhash-set-free (zhash key free-fn)
  (assert key)
  (let ((key->free (or (gethash nil zhash)
		       (setf (gethash nil zhash)
			     (make-hash-table :test 'equal)))))
    (setf (gethash key key->free) free-fn)))

(defun %zhash-get-free (zhash key)
  (let ((key->free (gethash nil zhash)))
    (when key->free
      (gethash key key->free))))

(defun %zhash-rem-free (zhash key)
  (let ((key->free (gethash nil zhash)))
    (when key->free
      (remhash key key->free)
      (when (zerop (hash-table-count key->free))
	(remhash nil zhash)))))

;;  --------------------------------------------------------------------------
;;  Local helper function
;;  Destroy item in hash table.

(defun %item-destroy (zhash key hard)
  (when hard
    (let ((free (%zhash-get-free zhash key)))
      (when free
	(funcall free (gethash key zhash)))))
  (%zhash-rem-free zhash key)
  (remhash key zhash))

;;  --------------------------------------------------------------------------
;;  Hash table constructor

(defun zhash-new ()
  (make-hash-table :test 'equal :size +initial-size+
		   :rehash-size (/ +growth-factor+ 100.0)
		   :rehash-threshold (/ +load-factor+ 100.0)))


;;  --------------------------------------------------------------------------
;;  Hash table destructor

(defun zhash-destroy (zhash)
  "Only necessary to call if items have free-fns."
  (when (gethash nil zhash) ;; there are free-fns.
    (loop for key being the hash-key of zhash
       when key
       do (%item-destroy zhash key t)))
  (clrhash zhash))

;;  --------------------------------------------------------------------------
;;  Insert item into hash table with specified key and item
;;  If key is already present returns nil and leaves existing item unchanged
;;  Returns t on success.

(defun zhash-insert (zhash key value)
  (assert key)
  (unless (gethash key zhash)
    (setf (gethash key zhash) value)
    t))

;;  --------------------------------------------------------------------------
;;  Update item into hash table with specified key and item.
;;  If key is already present, destroys old item and inserts new one.
;;  Use free_fn method to ensure deallocator is properly called on item.

(defun zhash-update (zhash key value)
  (assert key)
  (when (gethash key zhash)
    (%item-destroy zhash key t))
  (zhash-insert zhash key value))

;;  --------------------------------------------------------------------------
;;  Remove an item specified by key from the hash table. If there was no such
;;  item, this function does nothing.

(defun zhash-delete (zhash key)
  (assert key)
  (%item-destroy zhash key t))

;;  --------------------------------------------------------------------------
;;  Look for item in hash table and return its item, or NULL

(defun zhash-lookup (zhash key)
  (assert key)
  (gethash key zhash))

;;  --------------------------------------------------------------------------
;;  Reindexes an item from an old key to a new key. If there was no such
;;  item, does nothing. If the new key already exists, deletes old item.

(defun zhash-rename (zhash old-key new-key)
  (assert old-key)
  (assert new-key)
  (let ((val (gethash old-key zhash)))
    (unless val (return-from zhash-rename))
    (cond ((gethash new-key zhash)
	   (%item-destroy zhash old-key t)
	   nil)
	  (t
	   (let ((free-fn (%zhash-get-free zhash old-key)))
	     (when free-fn
	       (%zhash-set-free zhash new-key free-fn))
	     (%item-destroy zhash old-key nil)
	     (zhash-insert zhash new-key val))))))

;;  --------------------------------------------------------------------------
;;  Set a free function for the specified hash table item. When the item is
;;  destroyed, the free function, if any, is called on that item.
;;  Use this when hash items are dynamically allocated, to ensure that
;;  you don't have memory leaks. You can pass 'free' or NULL as a free_fn.
;;  Returns the item, or nil if there is no such item.

(defun zhash-freefn (zhash key free-fn)
  (assert key)

  (let ((item (gethash key zhash)))
    (when item
      (%zhash-set-free zhash key free-fn)
      item)))

;;  --------------------------------------------------------------------------
;;  Return size of hash table

(defun zhash-size (zhash)
  (let ((count (hash-table-count zhash)))
    (if (gethash nil zhash)
	(1- count) count)))

;;  --------------------------------------------------------------------------
;;  Make copy of hash table
;;  Does not copy items themselves. Rebuilds new table so may be slow on
;;  very large tables. NOTE: only works with item values that are strings
;;  since there's no other way to know how to duplicate the item value.

(defun zhash-dup (zhash)
  (loop with copy = (zhash-new)
     for key being the hash-key of zhash using (hash-value value)
     when key do (zhash-insert copy key value)
     finally (return copy)))

;;  --------------------------------------------------------------------------
;;  Return keys for items in table (as a zlist)

(defun zhash-keys (zhash)
  (loop with keys = (zlist-new)
     for key being the hash-key of zhash
     when key do
       (zlist-append keys key)
     finally (return keys)))


;;  --------------------------------------------------------------------------
;;;  Apply function to each item in the hash table. Items are iterated in no
;;  defined order.  Stops if callback function returns non-nil and returns
;;  final return code from callback function (nil = success).

(defun zhash-foreach (zhash callback &rest args)
  (loop for key being the hash-key of zhash using (hash-value value)
     when key do
       (let ((result (apply callback key value args)))
	 (when result (return result)))))

;;  --------------------------------------------------------------------------
;;  Save hash table to a text file in name=value format
;;  Hash values must be printable strings; keys may not contain '=' character
;;  Returns t if OK, else nil if a file error occurred

(defun zhash-save (zhash filename &key (if-exists :error))
  (ignore-errors
    (with-open-file (stream filename :direction :output :if-exists if-exists)
      (loop for key being the hash-key of zhash using (hash-value value)
	 when key do
	   (format stream "~A=~A~%" key value)
	 finally (return t)))))

;;  --------------------------------------------------------------------------
;;  Load hash table from a text file in name=value format; hash table must
;;  already exist. Hash values must printable strings; keys may not contain
;;  '=' character. Returns t if OK, else nil if a file was not readable.

(defun zhash-load (zhash filename)
  (ignore-errors
    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil nil)
	 while line do
	   (let ((equals (position #\= line)))
	     (unless equals
	       (return-from zhash-load)) ;; bail with error signal.
	     (let ((key (subseq line 0 equals))
		   (value (subseq line (1+ equals))))
	       (zhash-update zhash key value)))
	 finally (return t)))))

#||
//  --------------------------------------------------------------------------
//  Set hash for automatic value destruction

void
zhash_autofree (zhash_t *self)
{
    assert (self);
    self->autofree = true;
}
||#

;;  --------------------------------------------------------------------------
;;  Runs selftest of class
;;  TODO: add unit test for free_fn, foreach
;;

(defun test-foreach (key value zhash)
  (declare (ignore value))
  (assert (zhash-lookup zhash key))
  nil)

(defun test-foreach-error (key value zhash)
  (declare (ignore key value zhash))
  t)

(defun zhash-test (verbose)
  (declare (ignore verbose))
  (format t " * zhash: ")

  ;;  @selftest
  (let ((hash (zhash-new)))
    (assert hash)
    (assert (zerop (zhash-size hash)))

    ;;  Insert some items
    (assert (zhash-insert hash "DEADBEEF" "dead beef"))
    (assert (zhash-insert hash "ABADCAFE" "a bad cafe"))
    (assert (zhash-insert hash "C0DEDBAD" "coded bad"))
    (assert (zhash-insert hash "DEADF00D" "dead food"))
    (assert (= (zhash-size hash) 4))

    ;;  Look for existing items
    (assert (string= (zhash-lookup hash "DEADBEEF")
		     "dead beef"))
    (assert (string= (zhash-lookup hash "ABADCAFE")
		     "a bad cafe"))
    (assert (string= (zhash-lookup hash "C0DEDBAD")
		     "coded bad"))
    (assert (string= (zhash-lookup hash "DEADF00D")
		     "dead food"))

    ;;  Look for non-existent items
    (assert (null (zhash-lookup hash "foo")))

    ;;  Try to insert duplicate items
    (assert (null (zhash-insert hash "DEADBEEF" "foo")))
    (assert (string= (zhash-lookup hash "DEADBEEF")
		     "dead beef"))

    ;;  Rename an item
    (assert (zhash-rename hash "DEADBEEF" "LIVEBEEF"))
    (assert (null (zhash-rename hash "WHATBEEF" "LIVEBEEF")))

    ;;  Test keys method
    (let ((keys (zhash-keys hash)))
      (assert (= (zlist-size keys) 4)))

    ;;  Test dup method
    (let ((copy (zhash-dup hash)))
      (assert (= (zhash-size copy) 4))
      (assert (string= (zhash-lookup copy "LIVEBEEF")
		       "dead beef")))

    ;; Test foreach
    (assert (null (zhash-foreach hash #'test-foreach hash)))
    (assert (zhash-foreach hash #'test-foreach-error hash))

    ;;  Test save and load
    (zhash-save hash ".cache")
    (let ((copy (zhash-new)))
      (zhash-load copy ".cache")
      (let ((item (zhash-lookup copy "LIVEBEEF")))
	(assert item)
	(assert (string= item "dead beef"))))
    (delete-file ".cache")

    ;;  Delete an item
    (zhash-delete hash "LIVEBEEF")
    (assert (null (zhash-lookup hash "LIVEBEEF")))
    (assert (= (zhash-size hash) 3))

    ;;  Check that the queue is robust against random usage
    (let ((*random-state* (make-random-state t)))
      (loop
	 with testmax = 200
	 with testset = (make-array testmax :initial-element (cons nil nil))
	 for testnbr = (random testmax)
	 repeat 25000 do
	   (cond ((cdr (aref testset testnbr)) ;; exists
		  (let ((item (zhash-lookup hash (car (aref testset testnbr)))))
		    (assert item)
		    (zhash-delete hash (car (aref testset testnbr)))
		    (setf (cdr (aref testset testnbr)) nil)))
		 (t
		  (setf (aref testset testnbr)
			(cons (format nil "~x-~x" (random #x10000) (random #x10000))
			      nil)) ;; exists
		  (when (zhash-insert hash (car (aref testset testnbr)) "")
		    (setf (cdr (aref testset testnbr)) t)))))
      ;;  Test 10K lookups
      (loop repeat 10000 do
	   (zhash-lookup hash "DEADBEEFABADCAFE"))

      ;  Destructor should be safe to call twice
      (zhash-destroy hash)
      (zhash-destroy hash)

      (assert (zerop (zhash-size hash)))))
  ;;  @end

  (format t "OK~%"))
