/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.genifer;

import junit.framework.TestCase;

/**
 *
 * @author SEH
 */
public class TestMemory extends TestCase {
//;;; **** Initialize memories
//(defun init-memories ()
//  ;; Generic Memory
//  (setf *generic-memory* nil)
//  ;; index
//  (setf *memory-size* 0)
//  ;; counter used in batch processing
//  (setf *newly-added* 0)
//
//  (format t ";; Adding knowledge to KB.... ~%")
//
//  ;;; the format for facts is:
//  ;;;   (add-fact-to-mem (head) (TV) (justifies) (justified-by))
//  ;;;   where
//  ;;;      (TV) = (truth value . confidence)
//
//  ;;; the format for rules is:
//  ;;;   (add-rule-to-mem (head) (body) w+ w e+ e- ancestors ancestors-to)
//
//  ;;; ************* Example 1
//  ;;; This example occurs in this scenario:
//  ;;;   "I ask Kellie out for a date, but she says she is busy. Then, I find her drinking
//  ;;;   at the bar. Therefore, Kellie is lying."
//  ;;; It contains the simple rules:
//  ;;;   1. at_bar(x) -> having_fun(x)
//  ;;;   2. busy(x) -> ! having_fun(x)
//  ;;; and the following fact:
//  ;;;   busy(kellie)
//  ;;; The query would be:
//  ;;;   (at-bar kellie) ?
//  ;;; And the answer would be false (or its equivalent numerical truth value)
//
//  (add-fact-to-mem '(busy kellie) '(0.7 . 1.0))
//
//  (add-rule-to-mem '(having-fun ?1) '(Z-NOT (busy ?1)))
//  (add-rule-to-mem '(having-fun ?1) '(Z-MOD2 (at-bar ?1) 0.5 5.0))
//
//  ;;; ************* Example 2
//  ;;; Just a 6-layer-deep search problem with a distracting branch
//  ;;; Query:  (goal robot) ?
//  (add-fact-to-mem '(do-a robot) '(0.6 . 1.0))
//
//  (add-rule-to-mem '(do-b ?1) '(Z-MOD2 (do-a ?1) 0.5 5.0))
//  (add-rule-to-mem '(do-c ?1) '(Z-MOD2 (do-b ?1) 0.5 5.0))
//  (add-rule-to-mem '(do-d ?1) '(Z-MOD2 (do-c ?1) 0.5 5.0))
//  (add-rule-to-mem '(do-e ?1) '(Z-MOD2 (do-d ?1) 0.5 5.0))
//  (add-rule-to-mem '(do-f ?1) '(Z-MOD2 (do-e ?1) 0.5 5.0))
//  (add-rule-to-mem '(goal ?1) '(Z-MOD2 (do-f ?1) 0.5 5.0))
//  ; let's add a little distraction:
//  (add-rule-to-mem '(do-x ?1) '(Z-MOD2 (do-w ?1) 0.5 5.0))
//  (add-rule-to-mem '(do-y ?1) '(Z-MOD2 (do-x ?1) 0.5 5.0))
//  (add-rule-to-mem '(do-z ?1) '(Z-MOD2 (do-y ?1) 0.5 5.0))
//  (add-rule-to-mem '(goal ?1) '(Z-MOD2 (do-z ?1) 0.5 5.0))
//
//  ;;; ************* Example 3
//  ;;; c is a chair:
//  ;;;     chair(X) <- leg(1,X) & leg(2,X) & leg(3,X) & leg(4,X) & seat(X) & back(X)
//  ;;; Query:  (chair c) ?
//  (add-fact-to-mem '(leg 1 c) '(0.7 . 1.0))
//  (add-fact-to-mem '(leg 2 c) '(0.8 . 1.0))
//  (add-fact-to-mem '(leg 3 c) '(0.9 . 1.0))
//  (add-fact-to-mem '(leg 4 c) '(0.8 . 1.0))
//  (add-fact-to-mem '(seat  c) '(0.7 . 1.0))
//  (add-fact-to-mem '(back  c) '(0.9 . 1.0))
//
//  (add-rule-to-mem '(chair ?1) '(Z-AND (seat  ?1)
//                                       (back  ?1)
//                                       (leg 1 ?1)
//                                       (leg 2 ?1)
//                                       (leg 3 ?1)
//                                       (leg 4 ?1)))
//
//  ;;; A longer version using multiple intermediary predicates
//  ;;; Query:  (chair2 c) ?
//  (add-rule-to-mem '(chair2 ?1) '(Z-AND (seat  ?1) (tmp4  ?1)))
//  (add-rule-to-mem '(tmp4   ?1) '(Z-AND (back  ?1) (tmp3  ?1)))
//  (add-rule-to-mem '(tmp3   ?1) '(Z-AND (leg 1 ?1) (tmp2  ?1)))
//  (add-rule-to-mem '(tmp2   ?1) '(Z-AND (leg 2 ?1) (tmp1  ?1)))
//  (add-rule-to-mem '(tmp1   ?1) '(Z-AND (leg 3 ?1) (leg 4 ?1)))
//
//  ;;; ************* Example 4
//  ;;; From Luger's AI textbook, 2009
//  ;;; Query:  (happy john) ?
//
//  ;;; Anyone passing his history exams and winning the lottery is happy:
//  (add-rule-to-mem '(happy ?1) '(Z-AND (pass ?1 history) (win ?1 lottery)))
//
//  ;;; Anyone who studies or is lucky can pass all his exams:
//  (add-rule-to-mem '(pass ?1 ?2) '(Z-OR (study ?1) (lucky ?1)))
//
//  ;;; John did not study but is lucky:
//  (add-fact-to-mem '(study john) '(0.0 . 1.0))
//  (add-fact-to-mem '(lucky john))
//
//  ;;; Anyone who is lucky wins the lottery:
//  (add-rule-to-mem '(win ?1 lottery) '(ID (lucky ?1)))
//
//  ;;; ************* Example 5
//  ;;; Test handling of function symbols, ie unification.
//  ;;; Query:  (grandparent john ?1)
//
//  ;;; This is an example of a body-less rule:
//  ;;; parent(X, son-of(X)) <-
//  (add-rule-to-mem '(parent ?1 (son-of ?1)))
//
//  ;;; ~parent(W,Y) \/ ~parent(Y,Z) \/ grandparent(W,Z)
//  (add-rule-to-mem '(grandparent ?1 ?3) '(Z-AND (parent ?1 ?2) (parent ?2 ?3)))
//
//  ;;; ************* Example 6
//  ;;; Test of variable binding across a conjunction.
//  ;;;     p(X,Y) <- q(X,Z) /\ r(Z,Y)
//
//  ;;;     grandpa(X,Y) <- pa(X,Z) /\ pa(Z,Y)
//  ;;;     grandpa(X,Y) <- pa(X,Z) /\ ma(Z,Y)
//  ;;;     pa(john,pete)
//  ;;;     pa(pete,paul)
//  ;;;     ma(mary,paul)
//  ;;;     pa(john,mary)
//  ;;; Query: grandpa(john,paul)?
//  ;;; Query: grandpa(john,sam)?
//
//  (add-rule-to-mem '(grandpa ?1 ?2) '(Z-AND (pa ?1 ?3) (pa ?3 ?2)))
//  (add-rule-to-mem '(grandpa ?1 ?2) '(Z-AND (pa ?1 ?3) (ma ?3 ?2)))
//  (add-fact-to-mem '(pa john pete) '(1.0 . 1.0))
//  (add-fact-to-mem '(pa pete paul) '(1.0 . 1.0))
//  (add-fact-to-mem '(ma mary sam)  '(1.0 . 1.0))
//  (add-fact-to-mem '(pa john mary) '(1.0 . 1.0))
//
//  ;;; ************* Example 7
//  ;;; This background knowledge is used for testing induction in "induction1.lisp"
//  ;;; Query: (has-dau pam) ?   **** should return false
//
//  ;(add-rule-to-mem '(has-dau ?1) '(Z-AND (parent ?1 ?2) (female ?2)))
//
//  (add-fact-to-mem '(parent pam bob))
//  (add-fact-to-mem '(parent tom bob))
//  (add-fact-to-mem '(parent tom liz))
//  (add-fact-to-mem '(parent bob ann))
//  (add-fact-to-mem '(parent bob pat))
//  (add-fact-to-mem '(parent pat jim))
//  (add-fact-to-mem '(parent pat eve))
//  ;(add-fact-to-mem '(parent juu luu))
//
//  (add-fact-to-mem '(female pam))
//  (add-fact-to-mem '(male tom))
//  (add-fact-to-mem '(male bob))
//  (add-fact-to-mem '(female liz))
//  (add-fact-to-mem '(female ann))
//  (add-fact-to-mem '(female pat))
//  (add-fact-to-mem '(male jim))
//  (add-fact-to-mem '(female eve))
//  ;(add-fact-to-mem '(male juu))
//  ;(add-fact-to-mem '(male luu))
//
//  (format t ";; Working Memory initialized... ~%"))
//
//(defun system-test ()
//  (setf *abduce* nil)
//  (setf *debug-level* 10)
//  (format t "Time elapsed: ~1,15@T expected TV ~1,30@T confidence ~1,45@T substitutions~%")
//  ; format:  query expected-tv expected-sub
//  (test-1-query '(having-fun kellie) '(0.3 . 0.9) nil)         ; 1
//  (test-1-query '(at-bar kellie) '(0.3 . 0.9) nil)             ; 1 **** not yet ready
//  (test-1-query '(goal robot) '(0.9 . 0.9) nil)                ; 2
//  (test-1-query '(goal robot2) 'fail nil)                      ; 2
//  (test-1-query '(chair c) '(0.7 . 0.9) nil)                   ; 3
//  (test-1-query '(chair c2) 'fail nil)                         ; 3
//  (test-1-query '(chair2 c) '(0.7 . 0.9) nil)                  ; 3b
//  (test-1-query '(chair2 c2) 'fail nil)                        ; 3b
//  (test-1-query '(happy john) '(1.0 . 0.9) nil)                ; 4
//  ; 5
//  (test-1-query '(grandparent john ?99) '(1.0 . 0.9) '(?99 . (son-of (son-of john))))
//  (test-1-query '(grandparent john ?1) '(1.0 . 0.9) '(?99 . (son-of (son-of john))))
//  (test-1-query '(grandparent ?1 ?2) '(1.0 . 0.9) '())
//  (test-1-query '(grandparent ?1 john) 'fail nil)
//  ; 6
//  (test-1-query '(grandpa john paul) '(1.0 . 0.9) '(?a . pete))
//  (test-1-query '(grandpa john sam) '(1.0 . 0.9) '(?b . mary))
//  (test-1-query '(grandpa ?1 sam) '(1.0 . 0.9) '((?1 . john) (?a . mary)))
//)
//
//(defun test-1-query (query &optional expected-tv expected-sub)
//  (setf timer (get-internal-run-time))
//  (backward-chain query)
//  (setf solutions (solutions proof-tree))
//  (if (equal 'fail solutions)
//    (setf s1 nil)
//    (setf s1 (car solutions)))
//  (if (null s1)
//    (if (equal 'fail expected-tv)
//      (setf tv-accuracy         100.0
//            confidence-accuracy 100.0)
//      (setf tv-accuracy         0.0
//            confidence-accuracy 0.0))
//    (if (equal 'fail expected-tv)
//      (setf tv-accuracy         0.0
//            confidence-accuracy 0.0)
//      (progn
//        (setf tv-accuracy         (* 100.0 (- 1.0 (- (car (tv s1)) (car expected-tv)))))
//        (setf confidence-accuracy (* 100.0 (- 1.0 (- (cdr (tv s1)) (cdr expected-tv))))))))
//  (format t "~aus ~1,15@T ~a% ~1,30@T ~a% ~%" (- (get-internal-run-time) timer)
//                     tv-accuracy confidence-accuracy))

    
}
