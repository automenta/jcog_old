;;;; Genifer/unification.lisp
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
;;;; **** The 'unify' function is modified from Peter Novig's paper:
;;;; "Correcting A Widespread Error in Unification Algorithms"
;;;;    Software — Practice & Experience, vol 21:2, Feb 1991, p231-233

(defun unify (x y &optional subst)
  (cond ((equal x y) subst)
    ((equal subst 'fail) 'fail)
    ((var? x) (unify-variable x y subst))
    ((var? y) (unify-variable y x subst))
    ((or (atom x) (atom y)) 'fail)
    (t (unify (rest x) (rest y)
      (unify (first x) (first y) subst)))))

(defun unify-variable (var val subst)
  "Unify var with val, using (and possibly extending) subst."
  (cond ((equal var val) subst)
    ((bound? var subst)
    (unify (lookup var subst) val subst))
    ;; New condition: dereference val when it is a variable
    ((and (var? val) (bound? val subst))
    (unify var (lookup val subst) subst))
    ((occurs-in? var val subst) 'fail)
    (t (extend-subst var val subst))))

(defun occurs-in? (var x subst)
  "Does var occur anywhere inside x?"
  (cond ((equal var x) t)
    ((bound? x subst)
    (occurs-in? var (lookup x subst) subst))
    ((consp x) (or (occurs-in? var (first x) subst)
    (occurs-in? var (rest x) subst)))
    (t nil)))

(defun bound? (x subst)
  "Is x a bound variable?"
  (assoc x subst))

(defun lookup (var subst)
  (cdr (assoc var subst)))

(defun extend-subst (var val subst)
  (cons (cons var val) subst))

(defun var? (x)
  (if (atom x)
    (if (numberp x)
      nil
      (if (stringp x)
        nil
        (eq (char (symbol-name x) 0) #\?)))))   ; variable names start with '?'

;;; =====================================================================
;;; **** Factoring and Standardizing Apart
;;; (Standardizing apart is a trivial case of factoring)
;;; Find unifiable literals within a clause
;;; and make sure that c1 and c2 share no variables
;;; -- Maybe we can temporarily forget factoring?
;;;    assume there are no duplicate literals in a clause

;;; This is an index by which we create new unique variables as ?[index]
(defvar *unique-var-index* 1000)

;;; For accumulating results in find-all-vars
(defvar *new-vars* nil)

;;; Find all variables in clause
(defun find-all-vars (term)
  (if (null term)
    (return-from find-all-vars))
  (if (listp term)
    (progn
      (find-all-vars (car term))
      (if (not (null (cdr term)))
        (find-all-vars (cdr term))))
    (if (symbolp term)
      (let ((name (symbol-name term)))
        (if (eq (char name 0) #\?)
          (pushnew term *new-vars*))))))

;;; INPUT:  c2 = head of clause
;;;         c3 = body of clause
;;;         The head and body of clause is split because that's how we store them in rules
;;; OUTPUT: a list of substitutions such that, when applied, c2 and c3 will have fresh variables
;;; Algorithm:
;;; 1. find all vars in c2, c3
;;; 2. create unique subs for the vars

(defun standardize-apart (c2 c3)
  ;; Results will be accumulated in this variable:
  (setf *new-vars* nil)
  (find-all-vars c2)
  (find-all-vars c3)
  (create-unique-subs *new-vars*))

(defun create-unique-subs (clashed-vars)
  (mapcar
    (lambda (v) (cons v (intern (concatenate 'string "?"
                                   (write-to-string (incf *unique-var-index*))))))
    clashed-vars))

;;; ===============================================================================
;;; The following function is copied from [Steven L Tanimoto 1990]
;;; "The Elements of Artificial Intelligence -- Using Common Lisp", 1st ed, p230-232

;;; Apply substitutions subs-list to exp
(defun do-subst (exp subs-list)
  (cond ((null subs-list) exp)
        (t (subst (cdar subs-list)
                  (caar subs-list)
                  (do-subst exp (cdr subs-list))))))
