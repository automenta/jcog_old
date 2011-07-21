;;;; Genifer/induction2.lisp
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
;;;; **** Inductive learner II -- "Tracy"
;;;; Adapted from Bergadano & Gunetti's 1996 book "Inductive Logic Programming" ch.8

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
