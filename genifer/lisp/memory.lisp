;;;; Genifer/memory.lisp
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
;;;; ***** Memory systems
;;;; Currently the memory is just a simple list of all items, stored in *generic-memory*.

;;; **** Data structure for a Generic-Memory "fact" item
;;; Entries:
;;;     tv           = truth value and confidence
;;;     justifies    = a list of clauses that this fact justifies
;;;     justified-by = a list of clauses that justify this fact
;;; timestamp -- we should not timestamp for every clause, use time-markers instead
(defclass fact-item () (
  (fact         :initarg :fact          :accessor fact)
  (id           :initarg :id            :accessor id)
  (tv           :initarg :tv            :accessor tv            :initform '(1.0 . 1.0))
  (justifies    :initarg :justifies     :accessor justifies     :initform nil)
  (justified-by :initarg :justified-by  :accessor justified-by  :initform nil)
  ))

;;; **** Data structure for a Generic-Memory "rule" item
;;; Entries:
;;;     w            = size of support (ie, total number of times the rule is involved in proofs)
;;;     e+           = positive examples
;;;     e-           = negative examples
;;;     ancestors    = a list of ancestor rules of this rule
;;;     ancestors-to = a list of rules that this rule is ancestor to
(defclass rule-item () (
  (head        :initarg :head        :accessor head)
  (body        :initarg :body        :accessor body)
  (id          :initarg :id          :accessor id)
  (w           :initarg :w           :accessor w            :initform 100)
  (e+          :initarg :e+          :accessor e+           :initform nil)
  (e-          :initarg :e-          :accessor e-           :initform nil)
  (ancestors   :initarg :ancestors   :accessor ancestors    :initform nil)
  (ancestor-to :initarg :ancestor-to :accessor ancestor-to  :initform nil)
))

;;; **** Data structure for a rule / fact
;;; If it is a fact, body = nil
(defclass clause () (
  (id          :initarg :id          :accessor id)
  (confidence  :initarg :confidence  :accessor confidence)
  (head        :initarg :head        :accessor head)
  (body        :initarg :body        :accessor body        :initform nil)
  (tv          :initarg :tv          :accessor tv          :initform nil)
))

;;; Definition of the truth-value "T"
(defparameter *true* '(1.0 . 1.0))

(defvar *generic-memory*)
(defvar *memory-size*)
(defvar *newly-added*)
(defvar new-rule)

;;; **** Initialize memories
(defun init-memories ()
  ;; Generic Memory
  (setf *generic-memory* nil)
  ;; index
  (setf *memory-size* 0)
  ;; counter used in batch processing
  (setf *newly-added* 0)

  (format t ";; Adding knowledge to KB.... ~%")

  ;;; the format for facts is:
  ;;;   (add-fact-to-mem (head) (TV) (justifies) (justified-by))
  ;;;   where
  ;;;      (TV) = (truth value . confidence)

  ;;; the format for rules is:
  ;;;   (add-rule-to-mem (head) (body) w+ w e+ e- ancestors ancestors-to)

  ;;; ************* Example 1
  ;;; This example occurs in this scenario:
  ;;;   "I ask Kellie out for a date, but she says she is busy. Then, I find her drinking
  ;;;   at the bar. Therefore, Kellie is lying."
  ;;; It contains the simple rules:
  ;;;   1. at_bar(x) -> having_fun(x)
  ;;;   2. busy(x) -> ! having_fun(x)
  ;;; and the following fact:
  ;;;   busy(kellie)
  ;;; The query would be:
  ;;;   (at-bar kellie) ?
  ;;; And the answer would be false (or its equivalent numerical truth value)

  (add-fact-to-mem '(busy kellie) '(0.7 . 1.0))

  (add-rule-to-mem '(having-fun ?1) '(Z-NOT (busy ?1)))
  (add-rule-to-mem '(having-fun ?1) '(Z-MOD2 (at-bar ?1) 0.5 5.0))

  ;;; ************* Example 2
  ;;; Just a 6-layer-deep search problem with a distracting branch
  ;;; Query:  (goal robot) ?
  (add-fact-to-mem '(do-a robot) '(0.6 . 1.0))

  (add-rule-to-mem '(do-b ?1) '(Z-MOD2 (do-a ?1) 0.5 5.0))
  (add-rule-to-mem '(do-c ?1) '(Z-MOD2 (do-b ?1) 0.5 5.0))
  (add-rule-to-mem '(do-d ?1) '(Z-MOD2 (do-c ?1) 0.5 5.0))
  (add-rule-to-mem '(do-e ?1) '(Z-MOD2 (do-d ?1) 0.5 5.0))
  (add-rule-to-mem '(do-f ?1) '(Z-MOD2 (do-e ?1) 0.5 5.0))
  (add-rule-to-mem '(goal ?1) '(Z-MOD2 (do-f ?1) 0.5 5.0))
  ; let's add a little distraction:
  (add-rule-to-mem '(do-x ?1) '(Z-MOD2 (do-w ?1) 0.5 5.0))
  (add-rule-to-mem '(do-y ?1) '(Z-MOD2 (do-x ?1) 0.5 5.0))
  (add-rule-to-mem '(do-z ?1) '(Z-MOD2 (do-y ?1) 0.5 5.0))
  (add-rule-to-mem '(goal ?1) '(Z-MOD2 (do-z ?1) 0.5 5.0))

  ;;; ************* Example 3
  ;;; c is a chair:
  ;;;     chair(X) <- leg(1,X) & leg(2,X) & leg(3,X) & leg(4,X) & seat(X) & back(X)
  ;;; Query:  (chair c) ?
  (add-fact-to-mem '(leg 1 c) '(0.7 . 1.0))
  (add-fact-to-mem '(leg 2 c) '(0.8 . 1.0))
  (add-fact-to-mem '(leg 3 c) '(0.9 . 1.0))
  (add-fact-to-mem '(leg 4 c) '(0.8 . 1.0))
  (add-fact-to-mem '(seat  c) '(0.7 . 1.0))
  (add-fact-to-mem '(back  c) '(0.9 . 1.0))

  (add-rule-to-mem '(chair ?1) '(Z-AND (seat  ?1)
                                       (back  ?1)
                                       (leg 1 ?1)
                                       (leg 2 ?1)
                                       (leg 3 ?1)
                                       (leg 4 ?1)))

  ;;; A longer version using multiple intermediary predicates
  ;;; Query:  (chair2 c) ?
  (add-rule-to-mem '(chair2 ?1) '(Z-AND (seat  ?1) (tmp4  ?1)))
  (add-rule-to-mem '(tmp4   ?1) '(Z-AND (back  ?1) (tmp3  ?1)))
  (add-rule-to-mem '(tmp3   ?1) '(Z-AND (leg 1 ?1) (tmp2  ?1)))
  (add-rule-to-mem '(tmp2   ?1) '(Z-AND (leg 2 ?1) (tmp1  ?1)))
  (add-rule-to-mem '(tmp1   ?1) '(Z-AND (leg 3 ?1) (leg 4 ?1)))

  ;;; ************* Example 4
  ;;; From Luger's AI textbook, 2009
  ;;; Query:  (happy john) ?

  ;;; Anyone passing his history exams and winning the lottery is happy:
  (add-rule-to-mem '(happy ?1) '(Z-AND (pass ?1 history) (win ?1 lottery)))

  ;;; Anyone who studies or is lucky can pass all his exams:
  (add-rule-to-mem '(pass ?1 ?2) '(Z-OR (study ?1) (lucky ?1)))

  ;;; John did not study but is lucky:
  (add-fact-to-mem '(study john) '(0.0 . 1.0))
  (add-fact-to-mem '(lucky john))

  ;;; Anyone who is lucky wins the lottery:
  (add-rule-to-mem '(win ?1 lottery) '(ID (lucky ?1)))

  ;;; ************* Example 5
  ;;; Test handling of function symbols, ie unification.
  ;;; Query:  (grandparent john ?1)

  ;;; This is an example of a body-less rule:
  ;;; parent(X, son-of(X)) <-
  (add-rule-to-mem '(parent ?1 (son-of ?1)))

  ;;; ~parent(W,Y) \/ ~parent(Y,Z) \/ grandparent(W,Z)
  (add-rule-to-mem '(grandparent ?1 ?3) '(Z-AND (parent ?1 ?2) (parent ?2 ?3)))

  ;;; ************* Example 6
  ;;; Test of variable binding across a conjunction.
  ;;;     p(X,Y) <- q(X,Z) /\ r(Z,Y)

  ;;;     grandpa(X,Y) <- pa(X,Z) /\ pa(Z,Y)
  ;;;     grandpa(X,Y) <- pa(X,Z) /\ ma(Z,Y)
  ;;;     pa(john,pete)
  ;;;     pa(pete,paul)
  ;;;     ma(mary,paul)
  ;;;     pa(john,mary)
  ;;; Query: grandpa(john,paul)?
  ;;; Query: grandpa(john,sam)?

  (add-rule-to-mem '(grandpa ?1 ?2) '(Z-AND (pa ?1 ?3) (pa ?3 ?2)))
  (add-rule-to-mem '(grandpa ?1 ?2) '(Z-AND (pa ?1 ?3) (ma ?3 ?2)))
  (add-fact-to-mem '(pa john pete) '(1.0 . 1.0))
  (add-fact-to-mem '(pa pete paul) '(1.0 . 1.0))
  (add-fact-to-mem '(ma mary sam)  '(1.0 . 1.0))
  (add-fact-to-mem '(pa john mary) '(1.0 . 1.0))

  ;;; ************* Example 7
  ;;; This background knowledge is used for testing induction in "induction1.lisp"
  ;;; Query: (has-dau pam) ?   **** should return false

  ;(add-rule-to-mem '(has-dau ?1) '(Z-AND (parent ?1 ?2) (female ?2)))

  (add-fact-to-mem '(parent pam bob))
  (add-fact-to-mem '(parent tom bob))
  (add-fact-to-mem '(parent tom liz))
  (add-fact-to-mem '(parent bob ann))
  (add-fact-to-mem '(parent bob pat))
  (add-fact-to-mem '(parent pat jim))
  (add-fact-to-mem '(parent pat eve))
  ;(add-fact-to-mem '(parent juu luu))

  (add-fact-to-mem '(female pam))
  (add-fact-to-mem '(male tom))
  (add-fact-to-mem '(male bob))
  (add-fact-to-mem '(female liz))
  (add-fact-to-mem '(female ann))
  (add-fact-to-mem '(female pat))
  (add-fact-to-mem '(male jim))
  (add-fact-to-mem '(female eve))
  ;(add-fact-to-mem '(male juu))
  ;(add-fact-to-mem '(male luu))

  (format t ";; Working Memory initialized... ~%"))

(defun system-test ()
  (setf *abduce* nil)
  (setf *debug-level* 10)
  (format t "Time elapsed: ~1,15@T expected TV ~1,30@T confidence ~1,45@T substitutions~%")
  ; format:  query expected-tv expected-sub
  (test-1-query '(having-fun kellie) '(0.3 . 0.9) nil)         ; 1
  (test-1-query '(at-bar kellie) '(0.3 . 0.9) nil)             ; 1 **** not yet ready
  (test-1-query '(goal robot) '(0.9 . 0.9) nil)                ; 2
  (test-1-query '(goal robot2) 'fail nil)                      ; 2
  (test-1-query '(chair c) '(0.7 . 0.9) nil)                   ; 3
  (test-1-query '(chair c2) 'fail nil)                         ; 3
  (test-1-query '(chair2 c) '(0.7 . 0.9) nil)                  ; 3b
  (test-1-query '(chair2 c2) 'fail nil)                        ; 3b
  (test-1-query '(happy john) '(1.0 . 0.9) nil)                ; 4
  ; 5
  (test-1-query '(grandparent john ?99) '(1.0 . 0.9) '(?99 . (son-of (son-of john))))
  (test-1-query '(grandparent john ?1) '(1.0 . 0.9) '(?99 . (son-of (son-of john))))
  (test-1-query '(grandparent ?1 ?2) '(1.0 . 0.9) '())
  (test-1-query '(grandparent ?1 john) 'fail nil)
  ; 6
  (test-1-query '(grandpa john paul) '(1.0 . 0.9) '(?a . pete))
  (test-1-query '(grandpa john sam) '(1.0 . 0.9) '(?b . mary))
  (test-1-query '(grandpa ?1 sam) '(1.0 . 0.9) '((?1 . john) (?a . mary)))
  (setf *debug-level* 1)
)

(defun test-1-query (query &optional expected-tv expected-sub)
  (setf timer (get-internal-run-time))
  (backward-chain query)
  (setf solutions (solutions proof-tree))
  (if (equal 'fail solutions)
    (setf s1 nil)
    (setf s1 (car solutions)))
  (if (null s1)
    (if (equal 'fail expected-tv)
      (setf tv-accuracy         100.0
            confidence-accuracy 100.0)
      (setf tv-accuracy         0.0
            confidence-accuracy 0.0))
    (if (equal 'fail expected-tv)
      (setf tv-accuracy         0.0
            confidence-accuracy 0.0)
      (progn
        (setf tv-accuracy         (* 100.0 (- 1.0 (- (car (tv s1)) (car expected-tv)))))
        (setf confidence-accuracy (* 100.0 (- 1.0 (- (cdr (tv s1)) (cdr expected-tv))))))))
  (format t "~aus ~1,15@T ~a% ~1,30@T ~a% ~%" (- (get-internal-run-time) timer)
                     tv-accuracy confidence-accuracy))

;;; **** Add a clause to memory
;;; TODO:  label, and the stack from abduction
(defun add-to-memory (clause label)
  ;; check if the clause is already in memory -- this may be time-consuming.
  (dolist (item *generic-memory*)
    (if (equal clause (slot-value item 'clause))
      (return-from add-to-memory)))     ; if so, exit
  ;; if not in memory, add it
  (if (is-ground clause)
    (add-fact-to-mem clause)
    (add-rule-to-mem clause)))

(defvar new-fact)

;;; **** Add a fact to Generic Memory
(defun add-fact-to-mem (fact &optional tv justifies justified-by)
  (if (null tv)
    (setf tv (cons 1.0 1.0)))          ; default TV
  (****DEBUG 1 "adding fact to memory: ~a" fact)
  ;; create an object
  (setf new-fact (make-instance 'fact-item
                          :fact         fact
                          :id           *memory-size*
                          :tv           tv
                          :justifies    justifies
                          :justified-by justified-by))
  ;; increase the index
  (incf *memory-size*)
  (incf *newly-added*)
  ;; add the object to GM, by appending to the end of list
  (setf *generic-memory* (cons new-fact *generic-memory*)))

;;; **** Add a rule to Generic Memory
(defun add-rule-to-mem (head &optional body w e+ e- ancestors ancestor-to)
  ;; Set default values:
  (****DEBUG 1 "adding rule to memory: ~a <- ~a" head body)
  (if (null body) (setf body '(*bodyless*)))
  (if (null w )   (setf w    100))
  (if (null e+)   (setf e+   0))
  (if (null e-)   (setf e-   0))
  ;; create an object
  (setf new-rule (make-instance 'rule-item
                          :head         head
                          :body         body
                          :id           *memory-size*
                          :w            w
                          :e+           e+
                          :e-           e-
                          :ancestors    ancestors
                          :ancestor-to  ancestor-to))
  ;; increase the index
  (incf *memory-size*)
  (incf *newly-added*)
  ;; add the object to GM, by appending to the end of list
  (setf *generic-memory* (cons new-rule *generic-memory*)))

(defun delete-memory-item (item)
  (setf ptr *generic-memory*)
  ;; Special case:
  (if (eql item ptr)
    (return-from delete-memory-item
      (setf *generic-memory* (cdr *generic-memory*))))
  ;; Otherwise, find the item
  (loop
    (if (eql item (cdr ptr)) (return))
    (setf ptr (cdr ptr)))
  ;; Delete it
  (setf (cdr ptr) (cdr item)))

;;; Fetch all clauses in KB with the given head-predicate
;;; Return: a list of rules
(defun fetch-clauses (head-predicate)
  (let ((facts-list (list nil))
        (rules-list (list nil)))
    (dolist (item *generic-memory*)
      ;; is it a rule?
      (if (eql (type-of item) 'rule-item)
        (let ((head (head item))
              (body (body item)))
          ;; Does head of rule match head-predicate?
          (if (equal (car head) head-predicate)
            (progn
              ;; calculate the confidence c from w
              ;; the function is defined in "PZ-calculus.lisp"
              (setf confidence (convert-w-2-c (w item)))
              ;; add it to list-to-be-returned
              (nconc rules-list (list (make-instance 'clause
                                        :id         (id item)
                                        :confidence confidence
                                        :head       head
                                        :body       body))))))
        ;; If it is a fact:
        ;; Does fact match head-predicate?
        (if (equal (car (fact item)) head-predicate)
          (let ((tv (tv item)))
            ;; add it to list-to-be-returned
            (nconc facts-list (list (make-instance 'clause
                                      :id         (id item)
                                      :confidence (cdr tv)
                                      :head       (fact item)
                                      :tv         tv)))))))
    ;; return the 2 lists, discarding the leading 'nil' items
    (values (cdr facts-list) (cdr rules-list))))

;;; Comparison predicate for "sort" in function "fetch-clauses"
;;; should return true iff x1 is strictly less than x2
;;; if x1 is greater than or equal to x2, return false
;;; sort seems to order from small to big -- we need to reverse this -- biggest confidence 1st
;;; each element is a list:  (confidence head body)
(defun compare-items (x1 x2)
  (> (car x1) (car x2)))

;;; This function is used in natural-language.lisp
(defvar *entity-counter* 1)
;;; **** Creates a new entity
(defun new-entity ()
  (incf *entity-counter*))

;;; ------------------------ miscellaneous functions -------------------------

;;; **** Print out memory contents
(defun dump-memory ()
  (dolist (item *generic-memory*)
    (if (eql (type-of item) 'fact-item)
      ;; print a fact item
      (progn
        (format t "**** [~a] fact: ~a ~%" (id item) (fact item))
        (setf tv (tv item))
        (format t "  TV:           ~a ~%" (car tv))
        (format t "  confidence:   ~a ~%" (cdr tv))
        (format t "  justifies:    ~a ~%" (justifies    item))
        (format t "  justified-by: ~a ~%" (justified-by item)))
      ;; print a rule item
      (progn
        (format t "**** [~a] rule: ~%"    (id          item))
        (format t "  head:         ~a ~%" (head        item))
        (format t "  body:         ~a ~%" (body        item))
        (format t "  w:            ~a ~%" (w           item))
        (format t "  e+:           ~a ~%" (e+          item))
        (format t "  e-:           ~a ~%" (e-          item))
        (format t "  ancestors:    ~a ~%" (ancestors   item))
        (format t "  ancestors-to: ~a ~%" (ancestor-to item))))))
