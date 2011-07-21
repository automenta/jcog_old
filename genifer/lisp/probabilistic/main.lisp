;;;; Genifer/main.lisp
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

;;; change to the directory containing the code
;(cd "/cygdrive/c/lisp/Genifer")

;;; Logic-type can be:
;;;     "Z"    = pure fuzzy
;;;     "P"    = P(B) = binary probabilistic
;;;     "P(Z)" = fuzzy probabilistic
(defvar *logic-type* "P")

(defvar *debug-level* 1)

;;; TODO: breakpoint macro -- still buggy
(defun br (&optional msg)
  (setf ch (read-char *standard-input*))
  (if (not (eql ch #\Newline))
    (break msg)))

(defun ****BR (msg)
  (break (format nil
  "********************************************** ~a *******************************************"
  msg)))

(defun ****DEBUG (msg &optional arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)
  (if (numberp msg)
    (setf level msg)
    (setf level 10))
  (if (>= level *debug-level*)
    (progn
      (format t "DEBUG: ")
      (if (numberp msg)
        (format t arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)
        (format t msg arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10))
      (format t "~%"))))

;;; load modules
(load "memory.lisp")
(load "unification.lisp")
;(load "lazy-sequence.lisp")
(cond
  ((equalp *logic-type* "Z")
    (load "deduction-Z.lisp")
    (load "Z-calculus.lisp"))
  ((equalp *logic-type* "P")
    (load "Z-calculus.lisp")                          ; convert-w-to-C is needed
    (load "deduction-P.lisp"))
  (T
    (load "deduction-PZ.lisp")))
(load "induction1.lisp")
;(load "abduction.lisp")
;(load "induction.lisp")
;(load "coherence.lisp")
;(load "natural-language.lisp")
;(load "GUI.lisp")
(load "user-interface.lisp")    ; this must be loaded last
(main)