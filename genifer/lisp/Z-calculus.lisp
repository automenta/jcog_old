;;;; Genifer/deduction.lisp
;;;;
;;;; Copyright (C) 2009 Genint
;;;; All Rights Reserved
;;;;
;;;; Written by YKY
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License v3 as
;;;; published by the Free Software Foundation and including the exceptions
;;;; at http://opencog.org/wiki/Licenses
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program; if not, write to:
;;;; Free Software Foundation, Inc.,
;;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;;; ==========================================================
;;;; **** Z calculus
;;;; These are just mathematical formulae rendered in Lisp

;;; An example rule is:
;;;     (confidence (smart ?1) (Z-OR (creative ?1) (humorous ?1)))
;;; x1 and x2 each contains a truth value

;;; identity operator
(defun calculate-ID (cR x1)
  (cons
    (car x1)
    (min cR (cdr x1))))

;;; fuzzy Gamma #1
(defun calculate-Z-G1 (cR x1 k c)
  (let* ((z1 (car x1))
         (c1 (cdr x1))
         (delta (exp (/ -0.5 (* c c))))
         (tmp (- (expt z1 k) 0.5)))
    (cons
      (/ (- (exp (- (/ (* tmp tmp) (* c c)))) delta)
         (- 1.0 delta))
      (min cR c1))))

;;; fuzzy Gamma #2
(defun calculate-Z-G2 (cR x1 d k)
  (let ((z1 (car x1))
        (c1 (cdr x1))
        (L_-d (L-func (- d) k)))
    (cons
      (/ (- (L-func (- z1  d) k) L_-d)
         (- (L-func (- 1.0 d) k) L_-d))
      (min cR c1))))

;;; auxiliary function used by Gamma #2
(defun L-func (x k)
  (/ 1.0 (+ (exp (* (- k) x)) 1.0)))

;;; fuzzy NOT
(defun calculate-Z-NOT (cR x1)
  (let ((z1 (car x1))
        (c1 (cdr x1)))
    (cons
      (- 1.0 z1)
      (min cR c1))))

;;; fuzzy AND
(defun calculate-Z-AND (cR x1 x2)
  (let ((z1 (car x1))
        (z2 (car x2))
        (c1 (cdr x1))
        (c2 (cdr x2)))
    (cons
      (min z1 z2)
      (if (> z1 z2)
        (min cR c2)
        (min cR c1)))))

;;; fuzzy OR
(defun calculate-Z-OR (cR x1 x2)
  (let ((z1 (car x1))
        (z2 (car x2))
        (c1 (cdr x1))
        (c2 (cdr x2)))
    (cons
      (max z1 z2)
      (if (> z1 z2)
        (min cR c1)
        (min cR c2)))))

;;; This is tentative, not currently in use
;;; fuzzy combination (ie, affine combination)
(defun calculate-Z-combo (cR x1 x2 r1 r2)
  (let ((z1 (car x1))
        (z2 (car x2))
        (c1 (cdr x1))
        (c2 (cdr x2)))
    (cons
      (+ (* r1 z1) (* r2 z2))
      (/ (+ (* r1 c1) (* r2 c2) cR) 2.0))))

;;; Given w, calculate confidence c
(defun convert-w-2-c (w)
  (setf ratio (/ w W0))
  (- 1 (exp (* -0.69315 ratio ratio))))
