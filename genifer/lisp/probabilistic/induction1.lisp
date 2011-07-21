;;;; Genifer/induction1.lisp
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
;;;; **** Inductive learner I -- "Hyper"
;;;; An implementation of MINI-HYPER and HYPER from Ivan Bratko's 2001 book
;;;; "Prolog Programming for Artificial Intelligence" 3rd ed, ch.19

;;; ************************************* TO-DO *****************************************

;;; Examples should be stored as facts in "main memory".  For induction, we ask the prover if
;;; a hypothesis covers the example or not, ie query the example.  So it is as if the examples
;;; do not exist in the KB, ie their TVs can be set to 'fail.

; so perhaps we need a special func to explain a known fact?  which is a different operation than
; backward chaining?  Or we simply discard the fact that is being queried.

;;; Each hypothesis has a list of clauses
(defclass hypothesis () (
  (score-I :initarg :score-I :accessor score-I :initform 0)
  (clauses :initarg :clauses :accessor clauses :initform nil)
))

;;; "args" contains the variables that appear in the clause;  this is for programming convenience
(defclass clause-I () (
  (head-I :initarg :head-I :accessor head-I)
  (body-I :initarg :body-I :accessor body-I)
  (args-I :initarg :args-I :accessor args-I)
))

(defparameter *max-clause-length* 4)

(defvar clause-index 0)
(defvar *max-clause-index* 0)
(defvar arg-index 0)
(defvar *max-arg-index* 0)
(defvar literal-index 0)

(defparameter *starting-hypothesis*
  (make-instance 'hypothesis
    :score-I 0
    :clauses (list (make-instance 'clause-I
                     :head-I '(has-dau ?1)
                     :body-I nil
                     :args-I '(?1))))
;  (make-instance 'hypothesis
;    :score-I 0
;    :clauses (list (make-instance 'clause-I
;                     :head-I '(has-dau ?1)
;                     :body-I '(Z-AND (parent ?1 ?2) (female ?2))
;                     :args-I '(?1 ?2))))
)

(defparameter *background-literals* (list
(cons '(parent ?1 ?2)     '(?1 ?2))
(cons '(male   ?1)        '(?1))
(cons '(female ?1)        '(?1))
))

(defvar max-literal-index 3)

; Each clause is ((a list of literals) . (a list of variables in the clause))

;;; Please refer to example 6 for the background knowledge in "memory.lisp"

;;; Positive examples
(defvar *examples+* (list
   '(has-dau tom)
   '(has-dau bob)
   '(has-dau pat)
;   '(long-hair mary)
;   '(long-hair jane)
))

;;; Negative examples
(defvar *examples-* (list
   '(has-dau pam)
   '(has-dau jim)
;   '(long-hair john)
;   '(long-hair pete)
;   '(long-hair rita)
))

;;; Note:  due to the use of similar algorithmic ideas (eg best-first search), the procedures
;;; of this module will have the suffix '-I' to distinguish it from deduction.

;;; Best-first search algorithm:
;;; 1. If priority queue is empty, return fail
;;; 2. Remove first item from priority list and test it
;;; 3. Refine the current hypothesis
;;; 4. Merge results with priority queue
(defun induce ()
  (setf priority-list-I (list *starting-hypothesis*))
  (loop
    ;; 1. If priority queue is empty, return fail
    (if (null priority-list-I)
      (return 'fail))
    ;; 2. Remove first item from priority list and test it
    ;(print-priority-list-I)
    (setf best-I (car priority-list-I))
    (setf priority-list-I (cdr priority-list-I))
    ;; Test it
    (if (and (prove-complete   best-I)
             (prove-consistent best-I))
      (return best-I))
    ;(break "****************** tested 1 hype ********************")
    ;; 3. Refine the current hypothesis
    (setf refinements (refine-hyp best-I))
    ;; 4. Merge results with the priority queue
    (****DEBUG 2 "merging with P-queue")
    (sort refinements #'compare-scores-I)
    (setf priority-list-I (merge 'list refinements priority-list-I #'compare-scores-I))))

;;; Compare the scores of 2 priority-list items
(defun compare-scores-I (new old)
  (< (score-I new) (score-I old)))

;;; Find refinements of a hypothesis
;;; Return:  a list of refinements
(defun refine-hyp (hyp)
  ;; Initialize
  (setf results       (list nil)
        clause-index  0)
  ;; Select a clause from the hypothesis
  (dolist (clause (clauses hyp))
    ;; Try to make variable substitutions
    (****DEBUG 2 "Trying variable subs")
    (setf args (args-I clause))
    (setf rest-args args)
    (dolist (pivot1 args)
      (setf rest-args (cdr rest-args))
      (dolist (pivot2 rest-args)
        (if (not (equal pivot1 pivot2))
          (progn
            ;; Make a copy
            (setf hyp1 (make-copy hyp))
            ;; Find the current clause in copy
            (setf clause1 (nth clause-index (clauses hyp1)))
            ;; Make the substitution
            (setf sub (list (cons pivot2 pivot1)))
            (setf (head-I clause1) (do-subst (head-I clause1) sub))
            (setf (body-I clause1) (do-subst (body-I clause1) sub))
            ;; Delete the variable from the clause's variable list
            (setf (args-I clause1) (delete pivot2 (args-I clause1)))
            ;; Output as refined hypothesis:
            ;; Calculate new score
            (setf (score-I hyp1) (- (score-I hyp1) 1))
            (nconc results (list hyp1))))))
    ;; Try to add literals
    (****DEBUG 2 "Trying add literals")
    (dolist (literal *background-literals*)
      ;; Make a copy
      (setf hyp1 (make-copy hyp))
      ;; Find the current clause in copy
      (setf clause1 (nth clause-index (clauses hyp1)))
      ;; 'Standardize apart'
      (setf subs (standardize-apart-I clause1))
      (setf (head-I clause1) (do-subst (head-I clause1) subs))
      (setf (body-I clause1) (do-subst (body-I clause1) subs))
      (setf (args-I clause1) (do-subst (args-I clause1) subs))
      ;(****BR "huh")
      ;; Add literal to hypothesis
      (if (null (body-I clause))
        (setf   (body-I clause1) (list 'ID    (car literal)))
        (if (eql 'ID (car (body-I clause)))
          (setf (body-I clause1) (list 'Z-AND (second (body-I clause1)) (car literal)))
          (setf (body-I clause1) (list 'Z-AND (body-I clause1)          (car literal)))))
      (nconc (args-I clause1) (cdr literal))
      ;; Output as refined hypothesis:
      ;; Calculate new score
      (setf (score-I hyp1) (+ 10
                            (score-I hyp1)
                            (length (cdr literal))))
      (nconc results (list hyp1)))
    (incf clause-index))
  ;; Return the list -- first item is nil so it's discarded:
  (cdr results))

;;; 'Standardizing apart' -- see unification.lisp for details
(defun standardize-apart-I (clause)
  (setf *new-vars* nil)
  (find-all-vars (head-I clause))
  (find-all-vars (body-I clause))
  (create-unique-subs *new-vars*))

(defun make-copy (hyp)
  (setf clause-list (list nil))
  (dolist (clause (clauses hyp))
    (nconc clause-list (list (make-instance 'clause-I
                               :head-I (copy-tree (head-I clause))
                               :body-I (copy-tree (body-I clause))
                               :args-I (copy-tree (args-I clause))))))
  (make-instance 'hypothesis
    :score-I (score-I hyp)
    :clauses (cdr clause-list)))

;;; Prove that hyp covers all positive examples
(defun prove-complete (hyp)
  (setf mem-items-list (list nil))
  (dolist (clause (clauses hyp))
    (****DEBUG 2 "[~a] prove-complete: <~a> ~a <- ~a" ct (score-I hyp) (head-I clause) (body-I clause))
    (setf mem-item (add-rule-to-mem (head-I clause) (body-I clause)))
    (nconc mem-items-list (list mem-item)))
  (incf ct)
  (if (eql ct 0)  ;94
    (break))
  ;; For all positive examples...
  (dolist (e *examples+*)
    (backward-chain e)
    ;; Proof failed?
    (if (or (null (solutions proof-tree))
            (equal 'fail (solutions proof-tree)))
      ;; Return failure
      (progn
        (****DEBUG 2 "COMPLETENESS        ........failed: ~a" e)
        (setf (score-I hyp) (+ (score-I hyp) 30))
        (dolist (item (cdr mem-items-list))
          (delete-memory-item item))
        (return-from prove-complete nil))))
  (dolist (item (cdr mem-items-list))
    (delete-memory-item item))
  (****DEBUG 2 "COMPLETENESS        ........succeed")
  t)

(defvar ct 0)

;;; Prove that hyp does NOT cover any negative example
(defun prove-consistent (hyp)
  (setf mem-items-list (list nil))
  (dolist (clause (clauses hyp))
    (****DEBUG 2 "prove-consistent: <~a> ~a <- ~a" (score-I hyp) (head-I clause) (body-I clause))
    (setf mem-item (add-rule-to-mem (head-I clause) (body-I clause)))
    (nconc mem-items-list (list mem-item)))
  ;; For all negative examples...
  (dolist (e *examples-*)
    (backward-chain e)
    ;; Proof succeeded?
    (if (and (not (equal 'fail (solutions proof-tree)))
             (not (null (solutions proof-tree))))
      ;; Return failure
      (progn
        (****DEBUG 2 "CONSISTENCY        ........failed: ~a" e)
        (dolist (item (cdr mem-items-list))
          (delete-memory-item item))
        (return-from prove-consistent nil))))
  (dolist (item (cdr mem-items-list))
    (delete-memory-item item))
  (****DEBUG 2 "CONSISTENCY        ........succeed")
  t)

(defun print-priority-list-I ()
  (setf ptr priority-list-I)
  (loop
    (****DEBUG 2 "<~a>" (score-I (car ptr)))
    (dolist (clause (clauses (car ptr)))
      (****DEBUG 2 "------ ~a <- ~a [~a]" (head-I clause) (body-I clause) (args-I clause)))
    (setf ptr (cdr ptr))
    (if (null ptr) (return)))
  (****DEBUG 2 "~%"))

; (defun get-clause-length (clause)
;   (setf length 1
;         ptr    (second clause))
;   (loop
;     (if (equal (car ptr) 'Z-AND)
;       (incf length)
;       (return))
;     (setf ptr (cadr ptr)))
;   (****DEBUG "clause length = ~a" length)
;   length
; )
