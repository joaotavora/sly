;; This file is based very closely on a file that is part of GNU
;; Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code defines a ring data structure.  A ring is a
;;     (hd-index length . vector)
;; list.  You can insert to, remove from, and rotate a ring.  When the ring
;; fills up, insertions cause the oldest elts to be quietly dropped.
;;
;; In ring-ref, 0 is the index of the newest element.  Higher indexes
;; correspond to older elements; when the index equals the ring length,
;; it wraps to the newest element again.
;;
;; hd-index = vector index of the oldest ring item.
;;         Newer items follow this item; at the end of the vector,
;;         they wrap around to the start of the vector.
;; length = number of items currently in the ring.
;;         This never exceeds the length of the vector itself.
;;
;; These functions are used by the input history mechanism, but they can
;; be used for other purposes as well.
(defpackage :swank-ring
  (:export
   #:make-ring
   #:ring-insert-at-beginning
   #:ring-empty-p
   #:ring-size
   #:ring-copy
   #:ring-insert
   #:ring-remove
   #:ring-ref
   #:ring-elements
   #:ring-next
   #:ring-previous
   #:ring-extend
   #:ring-insert+extend
   #:ring-remove+insert+extend
   #:ring-convert-sequence-to-ring
   #:ring-rotate
   #:ring-member))
(in-package :swank-ring)

(defun ring-p (x)
  "Return t if X is a ring; nil otherwise."
  (and (consp x) (integerp (car x))
       (consp (cdr x)) (integerp (cadr x))
       (vectorp (cddr x))))

(defun make-ring (size)
  "Make a ring that can contain SIZE elements."
  (cons 0 (cons 0 (make-array size :initial-element nil))))

(defun ring-insert-at-beginning (ring item)
  "Add to RING the item ITEM, at the front, as the oldest item."
  (let* ((vec (cddr ring))
         (veclen (length vec))
         (hd (car ring))
         (ln (cadr ring)))
    (setq ln (min veclen (1+ ln))
          hd (ring-minus1 hd veclen))
    (setf (aref vec hd) item)
    (setf (car ring) hd)
    (setf (car (cdr ring)) ln)))

(defun ring-plus1 (index veclen)
  "Return INDEX+1, with wraparound."
  (let ((new-index (1+ index)))
    (if (= new-index veclen) 0 new-index)))

(defun ring-minus1 (index veclen)
  "Return INDEX-1, with wraparound."
  (- (if (zerop index) veclen index) 1))

(defun ring-length (ring)
  "Return the number of elements in the RING."
  (cadr ring))

(defun ring-index (index head ringlen veclen)
  "Convert nominal ring index INDEX to an internal index.
The internal index refers to the items ordered from newest to oldest.
HEAD is the index of the oldest element in the ring.
RINGLEN is the number of elements currently in the ring.
VECLEN is the size of the vector in the ring."
  (setq index (mod index ringlen))
  (mod (1- (+ head (- ringlen index))) veclen))

(defun ring-empty-p (ring)
  "Return t if RING is empty; nil otherwise."
  (zerop (cadr ring)))

(defun ring-size (ring)
  "Return the size of RING, the maximum number of elements it can contain."
  (length (cddr ring)))

(defun ring-copy (ring)
  "Return a copy of RING."
  (let ((vec (cddr ring))
	(hd  (car ring))
	(ln  (cadr ring)))
    (cons hd (cons ln (copy-seq vec)))))

(defun ring-insert (ring item)
  "Insert onto ring RING the item ITEM, as the newest (last) item.
If the ring is full, dump the oldest item to make room."
  (let* ((vec (cddr ring))
         (veclen (length vec))
         (hd (car ring))
         (ln (cadr ring)))
    (prog1
        (setf (aref vec (mod (+ hd ln) veclen)) item)
      (if (= ln veclen)
          (setf (car ring) (ring-plus1 hd veclen))
        (setf (car (cdr ring)) (1+ ln))))))

(defun ring-remove (ring &optional index)
  "Remove an item from the RING.  Return the removed item.
If optional INDEX is nil, remove the oldest item.  If it's
numeric, remove the element indexed."
  (if (ring-empty-p ring)
      (error "Ring empty")
      (let* ((hd (car ring))
             (ln (cadr ring))
             (vec (cddr ring))
             (veclen (length vec))
             (tl (mod (1- (+ hd ln)) veclen))
             oldelt)
        (when (null index)
          (setq index (1- ln)))
        (setq index (ring-index index hd ln veclen))
        (setq oldelt (aref vec index))
        (loop while (/= index tl)
              do
                 (setf (aref vec index) (aref vec (ring-plus1 index veclen)))
                 (setq index (ring-plus1 index veclen)))
        (setf (aref vec tl) nil)
        (setf (car (cdr ring)) (1- ln))
        oldelt)))

(defun ring-ref (ring index)
  "Return RING's INDEX element.
INDEX = 0 is the most recently inserted; higher indices
correspond to older elements.
INDEX need not be <= the ring length; the appropriate modulo operation
will be performed."
  (if (ring-empty-p ring)
      (error "Accessing an empty ring")
    (let ((hd (car ring))
	  (ln (cadr ring))
	  (vec (cddr ring)))
      (aref vec (ring-index index hd ln (length vec))))))

(defun ring-elements (ring)
  "Return a list of the elements of RING, in order, newest first."
  (let ((start (car ring))
	(size (ring-size ring))
	(vect (cddr ring))
	lst)
    (dotimes (var (cadr ring) lst)
      (push (aref vect (mod (+ start var) size)) lst))))

(defun ring-member (ring item &key (key #'identity) (test #'equal))
  "Return index of ITEM if on RING, else nil.
Comparison is done via `equal'.  The index is 0-based."
  (catch 'found
    (dotimes (ind (ring-length ring) nil)
      (let ((cand (ring-ref ring ind)))
        (when (and cand
                   (funcall test item (funcall key cand)))
          (throw 'found ind))))))

(defun ring-next (ring item &key (key #'identity) (test #'equal))
  "Return the next item in the RING, after ITEM.
Raise error if ITEM is not in the RING."
  (let ((curr-index (ring-member ring item key test)))
    (unless curr-index (error "Item is not in the ring: `%s'" item))
    (ring-ref ring (ring-plus1 curr-index (ring-length ring)))))

(defun ring-previous (ring item &key (key #'identity) (test #'equal))
  "Return the previous item in the RING, before ITEM.
Raise error if ITEM is not in the RING."
  (let ((curr-index (ring-member ring item key test)))
    (unless curr-index (error "Item is not in the ring: `%s'" item))
    (ring-ref ring (ring-minus1 curr-index (ring-length ring)))))

(defun ring-extend (ring x)
  "Increase the size of RING by X."
  (when (and (integerp x) (> x 0))
    (let* ((hd       (car ring))
	   (length   (ring-length ring))
	   (size     (ring-size ring))
	   (old-vec  (cddr ring))
	   (new-vec  (make-array (+ size x) :initial-element nil)))
      (setf (cdr ring) (cons length new-vec))
      ;; If the ring is wrapped, the existing elements must be written
      ;; out in the right order.
      (dotimes (j length)
	(setf (aref new-vec j) (aref old-vec (mod (+ hd j) size))))
      (setf (car ring) 0))))

(defun ring-insert+extend (ring item &optional grow-p)
  "Like `ring-insert', but if GROW-P is non-nil, then enlarge ring.
Insert onto ring RING the item ITEM, as the newest (last) item.
If the ring is full, behavior depends on GROW-P:
  If GROW-P is non-nil, enlarge the ring to accommodate the new item.
  If GROW-P is nil, dump the oldest item to make room for the new."
  (and grow-p
       (= (ring-length ring) (ring-size ring))
       (ring-extend ring 1))
  (ring-insert ring item))

(defun ring-remove+insert+extend (ring item &optional grow-p)
  "`ring-remove' ITEM from RING, then `ring-insert+extend' it.
This ensures that there is only one ITEM on RING.

If the RING is full, behavior depends on GROW-P:
  If GROW-P is non-nil, enlarge the ring to accommodate the new ITEM.
  If GROW-P is nil, dump the oldest item to make room for the new."
  (let (ind)
    (loop while (setq ind (ring-member ring item))
      do (ring-remove ring ind)))
  (ring-insert+extend ring item grow-p))

(defun ring-convert-sequence-to-ring (seq)
  "Convert sequence SEQ to a ring.  Return the ring.
If SEQ is already a ring, return it."
  (if (ring-p seq)
      seq
    (let* ((size (length seq))
           (ring (make-ring size)))
      (dotimes (count size)
        (when (or (ring-empty-p ring)
		  (not (equal (ring-ref ring 0) (elt seq count))))
	  (ring-insert-at-beginning ring (elt seq count))))
      ring)))


(defun ring-rotate (ring how-far-back)
  "Rotate RING to make the HOW-FAR-BACKth item the newest."
  (setf (car ring)
        (mod (- (ring-size ring)
                (nth-value 1
                           (truncate (+ (- (ring-size ring)
                                           (car ring))
                                        how-far-back)
                                     (ring-length ring))))
             (ring-size ring))))
