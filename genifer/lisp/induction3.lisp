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
;;;; **** Inductive learner III -- "Progol"

;;; ************************************* TO-DO *****************************************

;;;                        Note: currently this file contains *crap*

;;; ************************************* intro *****************************************
;;; The algorithm is bottom-up, based on generalization operators
;;; We can assume that prior to this call:
;;; 1. forward-chaining has been invoked on the incoming fact, ie, consequence finding
;;; 2. abduction has been invoked to explain the incoming fact
;;; We just learned that the incoming fact cannot be explained satisfactorily wrt the current KB
;;; So we would need to revise the KB or invent some rules to explain the incoming fact
;;; In other words, we search for a derivation that explains the fact
;;; The algorithm would be similar to abduction except that new rules may be created during the process
;;; So that means we backward chain with the negated incoming fact...?
(defun induce (incoming-fact)
  (write-line "Inductive learner invoked...")
  ;; Permute over all derivations...
  (loop
    ;; 1. Backward chain using 2 inverse operators:  inverse resolution and abduction
    ;;    Each step can be abductive or inductive.
    ;; select a clause from KB
    ;; if no more rules from KB, that means a dead end -- this can be an abducible
    ;;     if abducible, no more need to invent?
    ;; 2. Accumulate the steps, until the "successful explanation" criteria are met
    ;; 3. If criteria met, store the derivation in KB and invoke consistency check (type B)
    ;; 4. Criteria met or not met, now is the time to backtrack
  ;; then, this is the end.  Finished.
  ))

;;; A special mechanism is needed to *select* other facts/rules to incorporate into the new rule

;;; Find the bottom clause given an input clause c = "head <- body" w.r.t. background theory B
;;; 1.  Find a skolemization substitution for clause c (w.r.t. B and c)
;;; 2.  Find the least Herbrand model of B U ~body(c).theta
;;; 3.  Deskolemize the clause "head(c.theta) <- M" and return the result
(defun bottom (c)

)

; 1. c = long-hair(mary) <- nil
; 2. c = long-hair(X) <- female(X)
;    c = +ve(X) <- female(X),  ie   ~female(X) \/ +ve(X)

; 1. ~c = ~long-hair(mary)        ; subst = {}
; 2. ~c = female(sk) , ~+ve(sk)   ; subst = {X <- sk}

; Find least Herbrand model of:  B U ~c
; B contains:  long-hair(jane),  female(mary),  female(jane), female(rita), ~long-hair(rita)
; 1. ~long-hair(mary), nil
; 2. sk = roxane;  female(sk), ~long-hair(sk)

; deskolemize and construct "head <- M"
; 1. long-hair(mary) <- nil
; 2. +ve(X) <- female(X)

;;;    Search the hypothesis space using a refinement operator that considers only generalizations
;;; of the bottom clause.
