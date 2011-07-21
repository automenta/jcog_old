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


(defun ****DEBUG (msg &optional arg1 arg2 arg3 arg4 arg5)
  (if (numberp msg)
    (setf level msg)
    (setf level 10))
  (if (>= level *debug-level*)
    (progn
      (format t "DEBUG: ")
      (if (numberp msg)
        (format t arg1 arg2 arg3 arg4 arg5)
        (format t msg arg1 arg2 arg3 arg4 arg5))
      (format t "~%"))))

;;; load modules
(load "genifer/lisp/memory.lisp")
(load "genifer/lisp/unification.lisp")
;(load "lazy-sequence.lisp")
(load "genifer/lisp/deduction.lisp")
(load "genifer/lisp/Z-calculus.lisp")
(load "genifer/lisp/induction1.lisp")
;(load "abduction.lisp")
;(load "induction.lisp")
;(load "coherence.lisp")
;(load "natural-language.lisp")
;(load "GUI.lisp")
(load "genifer/lisp/user-interface.lisp")    ; this must be loaded last
(main)
