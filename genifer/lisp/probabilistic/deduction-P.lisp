;;;; =======================================================================
;;;;      Deduction: backward-chaining, using P(B) logic, best-first search
;;;; =======================================================================

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
;;;; ------------------------------------------------------------------------------

;;;; Abbreviations:
;;;;     sub           = substitution
;;;;     TV            = truth value

;;; ************************************* TO-DO *****************************************
;;; 0. factor graph calculations are wrong
;;;    a.  allow multiple-head rules -- OK
;;;    b.  store the way to calculate subgoal in the factor
;;; 1. using rules in the abductive direction
;;; 2. abduction
;;; 3. conjunctive head in rules
;;; 4. junction nodes (for cycle resolution)

;;; ********************************** Introduction ***************************************
;;;
;;; All truth values are formated as (P . confidence) where P is a point-valued probability
;;; All rules are all of the form:
;;;     ((head) (body))
;;; where
;;;     (head) = (predicate arg1 arg2 ...)
;;; note:  the # of args can be 0
;;; and
;;;     (body) = (theta c1 c2 ... X1 X2 ...)
;;; where theta is the operator, represented as a number in [0,1] where
;;;       theta = 0.0 or 1.0 corresponds to AND or OR
;;;       X1, X2, ... are the literals
;;;       c1, c2, ... are parameters associated with each literal
;;; Each literal can invoke other operators recursively

;;; For example:
;;;     smart <- creative \/ humorous
;;;     smart(?X) <- creative(?X) \/ humorous(?X)
;;; can be expressed as a P(B) rule as:
;;;     ((smart ?X) (1.0 0.9 0.9 (creative ?X) (humorous ?X)))

;;; The best-frist search algorithm can be implemented using call-by-continuations which
;;; allows remembering the search state, but we don't use that here.  We simply maintain
;;; the priority queue.

;;; ******************************* Define some parameters *******************************

(defparameter *max-depth*      7)     ; maximum search depth
(defparameter *max-rule-nodes* 150)   ; maximum number of rule nodes in proof tree
(defparameter max-rule-length  20)    ; rule length = # of arguments (literals) it has
(defparameter W0 50.0)                ; constant used in calculating confidence from w
(defparameter abduction-penalty 0.2)  ; < 1.0;  used to reduce the scores of abductive steps
(defparameter facts-boost       1.5)  ; > 1.0;  increases the scores of facts
(defparameter args-decay        0.8)  ; < 1.0;  decreases the scores of consecutive args in a rule
(defparameter testing-decay     0.98) ; < 1.0;  decreases scores of successive literal tests

(setf *print-circle* t)               ; Lisp key that allows printing of circular objects

;;; *********************************** Data structures **********************************
;;;
;;; Proof tree is a tree with back-links so we can go from a child node to its parent node.
;;; This is to facilitate the bottom-up evaluation of expressions, with the goal at the root.
;;; A tree node can be either a sub-goal or a fact/rule.
;;; Each tree node has a pointer to its parent.

;;; A sub-goal node contains a list in its "node-data" slot:
;;;     (sub-goal (child node1) (child node2) ...)

;;; A rule node contains a rule class item (see below) in the "node-data" slot;
;;; It has the following elements:
;;;     operator confidence literal1 [literal2...] [params...]
;;; where literal1,2,... are proof tree nodes and each has a pointer pointing to _this_ node.
;;; The parameters of the rule create a CPT (conditional probability table) which is then
;;;   represented as a factor (as in factor graph).

;;; A fact node contains the fact (as a list) in its "data" slot;

;;; solutions = probabilistic messages that pass from node to node in the factor graph algorithm
;;;   The idea originated from Judea Pearl's message-passing algorithm for Bayes nets
;;;   Each solution is also associated with a substitution (because each substitution instantiates
;;;       a distinct propositional sub-tree in the factor graph, and we use only one sub-tree to
;;;       represent all those instances)
;;;   So we can potentially have a list of solutions for each node, not just one solution per node
;;;   (In a more advanced version, such a list can be implemented as a lazy sequence)

;;; node-type can be:
;;;   For subgoal nodes: #\H = head literal or #\B = body literal, #\X = unknown (all else)
;;;   For rule    nodes: top's type = #\H or #\B or #\X (same as above)

(defclass tree-node () (
  (parent     :initarg :parent     :accessor parent                    :type symbol)
  (solutions  :initarg :solutions  :accessor solutions  :initform nil  :type (list solution))
  (node-data  :initarg :node-data  :accessor node-data  :initform nil  :type list)
  (node-type  :initarg :node-type  :accessor node-type  :initform #\X  :type char)
))

;;; Each solution is a pair:  (sub, message)
(defclass solution () (
  (sub      :initarg :sub      :accessor sub      :initform nil  :type list)
  (message  :initarg :message  :accessor message  :initform nil)
))

;;; Class for a rule data item within a proof tree node
;;; rule-type is either #\H or #\B depending if the top node is a head literal or body literal
(defclass rule () (
; (op         :initarg :op         :accessor op         :type single-float)
  (literals   :initarg :literals   :accessor literals   :type list           :initform nil)
  (parameters :initarg :parameters :accessor parameters :type (list single-float))
  (rule-type  :initarg :rule-type  :accessor rule-type  :type char           :initform #\X)
  (confidence :initarg :confidence :accessor confidence :type single-float)
  (factor     :initarg :factor     :accessor factor     :type single-float)
))

;;; An item of the priority list.
;;;     list-data = either a sub-goal or a fact/rule
;;;     ptr       = pointer to proof-tree node
;;;     data-type = rule-type, see above
(defclass p-list-item () (
  (score     :initarg :score      :accessor score      :type single-float  :initform 0.0)
  (depth     :initarg :depth      :accessor depth      :type fixnum        :initform 0)
  (list-data :initarg :list-data  :accessor list-data  :type symbol)
  (data-type :initarg :data-type  :accessor data-type  :type char          :initform #\X)
  (ptr       :initarg :ptr        :accessor ptr        :type symbol)
))

(defvar proof-tree      nil)         ; The initial proof-tree
(defvar priority-list   nil)
(defvar new-states-list nil)
(defvar *explanation*   nil)         ; the result of abduction
(defvar *abduct-mode*   nil)         ; true if abduction mode is ON

(defvar timer             0)
(defvar *goal-nodes*      0)
(defvar *rule-nodes*      0)
(defvar *fact-nodes*      0)

;;; NOTE:  the abduction algorithm is embedded in this code.
;;; TO-DO: explain abduction algorithm here

;;; **** Best-first search algorithm:
;;; ===============================================
;;; 0. Loop forever:
;;; 1.     If priority queue is empty, return fail
;;; 2.     Remove first item from priority list (the item points to a node in the proof tree)
;;; 3.     Go to the proof tree and process the node;
;;;            This results in some new child nodes of the proof tree node
;;; 4.     Put the new nodes into priority queue (via ranking-based merging)
;;; ===============================================
;;; About the scoring of the priority list:
;;;    There are 3 types of items in the priority list:  subgoals, rules, and facts;
;;;    Each has some factors that can be taken into consideration when calculating the score:
;;;    A. rule -- length, confidence, depth
;;;    B. fact -- confidence, depth
;;;    C. subgoal -- order within its rule, assuming most-significant first
(defun backward-chain (query)
  ;; 0. Initialize
  (setf proof-tree (make-instance 'tree-node :parent nil))
  (setf *goal-nodes*  1
        *rule-nodes*  0
        *fact-nodes*  0)
  ;; On entry, priority list contains the goal query
  (setf priority-list (list (make-instance 'p-list-item
                              :score      0.0
                              :depth      0
                              :list-data  query
                              :ptr        proof-tree)))
  (loop
    ;; Has found result?
    (if (or (not (null (solutions proof-tree)))
            (equal 'fail (solutions proof-tree)))
      (return))
    ;(if (> (- (get-internal-run-time) timer) 10000000)
    ;   (return))
    ;; 1. Remove first item from priority list
    (if (null priority-list)
      (return))
    (setf best (car priority-list))               ; "best" = top item on priority list
    (setf priority-list (cdr priority-list))
    ;; The current tree node being processed:
    (setf node (ptr best))
    ;; Maximum search depth reached?
    (setf depth (depth best))
    (if (> depth *max-depth*)
      (progn (****DEBUG 3 "backward-chain: FAIL -- past max tree depth")
        (return 'fail)))
    (if (> *rule-nodes* *max-rule-nodes*)
      (progn (****DEBUG 3 "backward-chain: FAIL -- past max num of rules")
        (return 'fail)))
    (if (> *goal-nodes* *max-rule-nodes*)
      (progn (****DEBUG 3 "backward-chain: FAIL -- past max num of goals")
        (return 'fail)))
    (****DEBUG 1 "~%")
    (****DEBUG 1 "backward-chain: proof-tree node = ~a" (node-data node))
    ;; 2. Process the current node, which can be a sub-goal, a rule, or a fact:
    (setf new-states-list (list nil))
    ;; Is it a sub-goal?
    (if (listp (list-data best))
      (process-subgoal node best)
      ;; Else... it is either a fact/rule
      (let ((formula (list-data best)))
        ;; Is it a fact?
        (if (null (body formula))
          (process-fact node formula best)
          ;; It's a rule:
          (process-rule node formula best))))
    ;; 3. Merge results with priority queue
    ;;    Results are stored in "new-states-list"
    (setf new-states-list (cdr new-states-list))   ; remove first element which is a dummy nil
    (sort new-states-list #'compare-scores)
    (****DEBUG 1 "backward-chain: new-states-list has ~a element(s)" (length new-states-list))
    (setf priority-list (merge 'list new-states-list priority-list #'compare-scores))
    (print-priority-list)
    (print-proof-tree)))

;;; compare the scores of 2 priority-list items
(defun compare-scores (new old)
  (> (score new) (score old)))

;;; **** Process a sub-goal node of the proof tree:
;;; On entry, "node" is the proof tree node being processed
;;; 1. Fetch facts / rules that match the subgoal
;;; 2. For each applicable rule / fact:
;;; 3.     Create a corresponding node in proof tree, as a child of the current node
;;; 4. Prepare the new nodes to be pushed to priority list
(defun process-subgoal (node best)
  (setf best-data (list-data best))
  (****DEBUG 1 "process-subgoal: sub-goal ~a" best-data)
  ;; store the sub-goal in the proof tree node
  (setf (node-data node) (list best-data))
  (setf (node-type node) (data-type best))
  ;; Fetch rules that match the subgoal:
  ;; 'fetch-rules' is defined in memory.lisp
  (multiple-value-bind (facts-list rules-list) (fetch (car best-data))
    (dolist (formula facts-list)
      ;; standardize apart head-of-subgoal and formula-to-be-added
      (setf subs (standardize-apart (head formula) nil)
                 (head formula) (do-subst (head formula) subs)
                 (body formula) nil)
      (****DEBUG 1 "process-subgoal: adding fact... ~a" (head formula))
      ;; create proof tree node for the fact, with initial tv = nil
      (setf new-node (make-instance 'tree-node :parent node))
      (incf *fact-nodes*)
      ;; add node as current node's child
      (nconc (node-data node) (list new-node))
      ;; prepare priority list item:
      ;; score := confidence * (1 - partial_length / max_length)
      (setf depth (depth best))
      (setf score (* facts-boost (confidence formula) (- 1.0 (/ depth *max-depth*))))
      (setf new-state (make-instance 'p-list-item
                        :score     score
                        :depth     (+ 1 depth)
                        :list-data formula
                        :ptr       new-node))
      (nconc new-states-list (list new-state)))
    ;; Do the same for rules:
    (dolist (formula rules-list)
      ;; standardize apart head-of-subgoal and formula-to-be-added
      (setf subs (standardize-apart (head formula) (body formula))
                 (head formula) (do-subst (head formula) subs)
                 (body formula) (do-subst (body formula) subs))
      (setf body (body formula))
      (****DEBUG 1 "process-subgoal: adding rule... ~a <- ~a" (head formula) body)
      ;; create proof tree node and store rule-formula in it
      (incf *rule-nodes*)
      (setf new-node (make-instance 'tree-node
                       :parent     node
                       :node-data  (make-instance 'rule
                                      :parameters (if (numberp body)    ; bodyless rule?
                                                    nil
                                                    (list (car body)))  ; first element = theta
                                      :confidence (confidence formula)
                                      :literals   nil)))
      ;; Add node as current node's child
      (nconc (node-data node) (list new-node))
      ;; Prepare priority list item:
      (setf depth (depth best))
      (if (numberp body)
        (setf score  (* (confidence formula) (- 1.0 (/ depth *max-depth*))))
        (setf score  (* (confidence formula) (- 1.0 (/ depth *max-depth*))
                                             (- 1.0 (/ (length body) max-rule-length))
                                             (if (> (top-index formula) 0)
                                               abduction-penalty
                                               1.0))))
      (setf new-state (make-instance 'p-list-item
                        :score     score
                        :depth     (+ 1 depth)
                        :list-data formula
                        :ptr       new-node))
      (nconc new-states-list (list new-state)))))

;;; **** Process a fact node of the proof tree:
;;; On entry, node points to the fact node in proof tree
;;; 0. Try to unify the fact with its parent node
;;; 1. IF unify() succeeds:
;;; 2.     add the sub to the node;
;;; 3.     send the resulting sub up to parent node
;;; 5.     update the entire proof tree in a bottom-up manner (by calling propagate)
;;; 6. ELSE              ; unify() fails
;;; 7.     no substitution is added
;;; 8.     the node can be deleted
(defun process-fact (node formula best)
  (****DEBUG 1 "process-fact: processing fact formula: ~a" (head formula))
  ;; Try to unify the fact with the parent node (the latter is a sub-goal):
  (****DEBUG 1 "process-fact: to unify with parent: ~a" (car (node-data (parent node))))
  (setf sub (unify (head formula) (car (node-data (parent node)))))
  ;; If unify fails, retract the proof tree node
  (if (eq sub 'fail)
    (progn
      (decf *fact-nodes*)
      (retract node best)
      (return-from process-fact)))
  ;; If unify succeeds:
  (****DEBUG 1 "process-fact: substitution = ~a" sub)
  ;; Store the (sub, message) pair in the list of solutions in the current node
  ;; Here we pass the first message from a leaf node
  ;; Note that there is an implicit factor node below this leaf, but it simply passes the
  ;;    probability of the variable to its parent.  (first TV) = probability value
  (setf current-solution (make-instance 'solution
                             :sub     sub
                             :message (first (tv formula))))
  ;; The current node is filled with the literal (this is useful for abduction; see below)
  (setf (node-data node) (list 'fact (head formula)))
  ;; Record the node for abduction
  ;(setf *current-explanation* (list 'fact (head formula)))
  ;; Propagate the TV bottom-up, recursively
  (propagate node (list current-solution)))

;;; **** Process a rule node of the proof tree:
;;; INPUT:  formula is a mem-item
;;; 1. Try to match parent node with head of rule.
;;; 2. IF unify succeeds:
;;; 3.     Set up the proof tree sub-nodes (= literals of the rule)
;;; 4.     Return the subgoals (= arguments = antecedents) to be pushed to priority list
;;; 5. ELSE:
;;; 6.     retract the node
(defun process-rule (node formula best)
  (****DEBUG 1 "process-rule: expanding rule formula ~a <- ~a" (head formula) (body formula))
  (setf head       (head formula)
        body       (body formula)
        top-index  (top-index formula))
  ;; At this point, a designated part of the rule (let's call it 'top') is already (partly)
  ;;   matched with the parent.  The parent is a subgoal.  We need to perform unify on them.
  (****DEBUG 1 "process-rule: to unify with parent: ~a" (car (node-data (parent node))))
  ;; top = the rule's actual "head" per this instantiation
  ;; if top-index > 0, top is a body literal; otherwise top is a head literal
  (if (> top-index 0)
    (setf top (nth (+ top-index (/ (- (length body) 1) 2)) body))
    (setf top (nth (- top-index)   head)))
  (****DEBUG 1 "process-rule: to unify with index: ~a, top: ~a" top-index top)
  (setf sub (unify top (car (node-data (parent node)))))
  (****DEBUG 1 "process-rule: substitution = ~a" sub)
  ;; If unify fails, retract proof tree node
  (if (eq sub 'fail)
    (progn
      (decf *rule-nodes*)
      (retract node best)
      (return-from process-rule)))
  ;; If unify succeeds:
  ;; We don't have to apply sub to the rule's top b/c the top is identical with the parent
  ;;    and is not stored in the rule.
  ;; If the rule is body-less, we immediately have a solution that should be propagated up
  ;;    to the parent:
  (if (numberp body)
    (progn
      (setf current-solution (make-instance 'solution
                                  :sub      sub
                                  :message  body))                  ; the TV is the body
      (propagate node (list current-solution))
      (return-from process-rule)))
  (if (> top-index 0)
    (setf (node-type node) #\B)
    (setf (node-type node) #\H))
  ;; For each parameter c or body literal...
  (setf top-index2 1)
  (dolist (arg (cdr body))
    ;; Auxiliary parameters?
    (if (floatp arg)
      ;; Yes, add them to parameter list
      ;; The original order should be preserved regardless of where top-index is.
      (setf (parameters (node-data node)) (append (parameters (node-data node)) (list arg)))
      ;; For each literal:
      (progn
        (if (not (equal top-index top-index2))
          (progn
            ;; Apply substitution to literal:
            (setf lit (do-subst arg sub))
            ;; Create new nodes for the proof-tree, corresponding to arguments of the rule:
            ;; The operator and confidence are already in the proof-tree node
            (incf *goal-nodes*)
            (setf new-lit (make-instance 'tree-node :parent node))
            ;; Add new lit to current node's literals list
            (setf (literals (node-data node)) (append (literals (node-data node)) (list new-lit)))
            ;; Prepare item for priority list:
            ;; note:  depth need not increase
            ;;        score is inherited from the rule's, multiplied by a consecutive decay factor
            (nconc new-states-list (list (make-instance 'p-list-item
                                           :score     (* args-decay (score best))
                                           :depth     (depth best)
                                           :list-data lit
                                           :data-type #\B               ; #\B = body literal
                                           :ptr       new-lit)))))
        (incf top-index2))))
  ;; For each head literal...
  (setf top-index2 0)              ; head index counts from 0 downwards
  (dolist (arg head)
    (if (not (equal top-index top-index2))
      (progn
        ;; Apply substitution to literal
        (setf lit (do-subst arg sub))
        ;; Create new node
        (incf *goal-nodes*)
        (setf new-lit (make-instance 'tree-node :parent node))
        ;; Add new lit to current node's literals list
        (setf (literals (node-data node)) (append (literals (node-data node)) (list new-lit)))
        ;; Prepare item for priority list
        (nconc new-states-list (list (make-instance 'p-list-item
                                           :score     (* args-decay (score best))
                                           :depth     (depth best)
                                           :list-data lit
                                           :data-type #\H               ; #\H = head literal
                                           :ptr       new-lit)))))
    (decf top-index2)))

;;; **** Try to retract (delete) a failed node
;;; On entry: current node has failed
;;; 1. remove the node
;;; 2. IF parent is a goal:
;;; 3.     parent has no child?
;;; 4.     if so, recurse to remove parent
;;; 5. IF parent is a rule:
;;; 6.     the entire rule needs to be removed
(defun retract (node best)
  (****DEBUG 1 "retracting node...")
  (setf parent (parent node))
  ;; The root node failed?
  (if (null parent)
    (progn
      (setf (solutions proof-tree) 'fail)
      (return-from retract)))
  ;; Is parent a rule or subgoal?
  (if (listp (node-data parent))
    ;; If parent is a sub-goal:  delete the node itself (ie the parent's child);
    ;; If the parent becomes empty then recurse
    (progn
      ;; Delete the node:  destructively modify the parent
      ;(decf *rule-nodes*)
      (setf (node-data parent) (delete node (node-data parent) :count 1))
      ;; Is parent empty now?  If so, delete it too
      (if (null (third (node-data parent)))       ; third item is the subgoal's first argument
        ;; recurse
        (retract parent best))
    )
    ;; If parent is a rule: delete parent
    (retract parent best)
  ))

;;; **** Propagate messages up the proof tree
;;; INPUT:   a new list of solutions arrives at the current node
;;; RETURN:  nothing (update solutions in proof tree)
;;; Algorithm:
;;; 0.  IF  we have reached root of the proof tree:
;;; 1.      return a list of new solutions
;;; 2.  IF  parent is a sub-goal:
;;;         here we potentially have multiple answers to the sub-goal...
;;; 3.      First, determine if any new solution is potentially competing with existing ones
;;;             (ie, referring to the same instance)
;;; 4.      IF       a solution is not competing with others, send it up to parent
;;; 5.      ELSE apply mixture rule, such as NARS' rule or Ben's rule;  send result to parent
;;;                                                        w1 f1 + w2 f2
;;;                  This is Pei Wang's formula:    f0 = -----------------
;;;                                                           w1 + w2
;;; **** Note:  In the current code, we don't do 3-5.
;;; **** We simply send multiple solutions to the parent, even if they are conflicting.
;;; **** The message to be sent = [Product of] incoming messages
;;; ****                          but the product usually has only 1 factor in it
;;; ****                          this is because we treat multiple rules as independent.
;;; 7.  IF  parent is a rule:
;;;
;;;                         O    parent node
;;;                        / \    is a rule
;;;                       /   \
;;;             current  O     O    sibling  . . .
;;;              node               node(s)
;;;
;;; 8.      try to apply the rule if all required arguments in the conjunction are available;
;;;                          ie, match the newly added sub against the subs of sibling nodes
;;; 9.      IF  a consistent sub is found:
;;; 10.         calculate:  message = local factor * all incoming messages
;;;             send the resulting message to parent node
;;;     [ it is impossible for the parent to be a fact ]
;;; 11. recurse to parent

(defun propagate (node new-solutions)
  (****DEBUG 1 "propagate: evaluating node: data = ~a" (node-data node))
  (setf parent    (parent node))
  ;; Add the new solutions to the current node
  (setf (solutions node) (append (solutions node) new-solutions))
  ;; Reached root node?
  (if (null parent)
    (progn
      (****DEBUG 1 "propagate: got solution = ~a" (print-solutions new-solutions))
      (return-from propagate)))
  (setf parent-data (node-data parent))
  ;; Is parent a sub-goal?
  (if (listp parent-data)
    ;; 3-6. Simply send all solutions to parent
    ;;      It seems that only the new solutions need to be sent up
    (propagate parent new-solutions)
    ;; ELSE parent is a rule:  try to apply the rule
    (let* ((rule     parent-data)
           (cR       (confidence rule))
           (theta    (car (parameters rule)))
           (literals (literals   rule)))
      (****DEBUG 1 "propagate: evaluating rule... op = ~a" theta)
      (****DEBUG 1 "propagate: rule type = ~a" (node-type parent))
      (if (equal (node-type parent) #\H)
        (tagbody
        case1A
          ;; case 1A: all Bi's are available
          ;; The strategy is to apply the AND/OR *pairwise*, sequentially.
          (setf c_list (cdr (parameters rule)))         ; List of parameters c1,c2,...
          (setf c1 (car c_list))                        ; Set c1
          (setf c_list (cdr c_list))                    ; Point to next element
          ;; First set up the left operand, X1
          (setf X1 (car literals))                      ; X1 = the first literal
          (if (eql X1 node)                             ; Is it the node with new solutions?
              (setf X1-solutions new-solutions)         ; If so, process new-solutions *only*
            (setf X1-solutions (solutions X1)))         ; If not, get the set of old solutions
          (prepare-left-operand c1 X1-solutions)        ; Prepare X1
          (****DEBUG 1 "propagate: literals = ~a" literals)
          ;; For each of the rest of siblings, X2:
          (dolist (X2 (cdr literals))
            ;; If some silbing has null TV, this means case 1A is broken, try case 1B
            (if (null (solutions x2))
              (go case1B))
            ;; If this is a "head" literal, it means the end of body literals, signals success
            (if (equal (node-type X2) #\H)
              (return))
            (if (eql X2 node)                           ; Is it the node with new solutions?
                (setf X2-solutions new-solutions)       ; If so, process new-solutions *only*
              (setf X2-solutions (solutions X2)))       ; If not, get the set of old solutions
            (setf c2 (car c_list))                      ; Set c2
            (setf c_list (cdr c_list))                  ; Point to next element
            ;; Merge with sibling's sequence:
            (setf X1-solutions
              (merge-solutions c2 X1-solutions X2-solutions))
            ;; Abduction mode?  **** TO-DO: abduction is unfinished
            (if *abduct-mode*
              ;; record the 2 arguments as parts of a potential explanation
              (setf *current-explanation* (list op *current-explanation* arg))))
          ;; If new-solutions is not empty
          (if (not (null X1-solutions))
            (progn
              ;; apply "theta":  result = (1 - theta) * AND-factor + theta * OR-factor
              (apply-theta theta X1-solutions)
              (****DEBUG 1 "propagate: got first TV = ~a" (message (car X1-solutions)))
              ;; send new-solutions to parent node;  recurse
              (propagate parent X1-solutions)))
          (return-from propagate)
        case1B
          ;; Case 1B:  apply Bayes rule (as in case 2) and then do forward (as in case 1A)
          (****DEBUG 1 "propagate: case 1B")
        )
        ;; case 2:  reverse direction; apply Bayes rule
        (progn
          (****DEBUG 1 "propagate: case 2")
          (setf c_list (cdr (parameters rule)))         ; List of parameters c1,c2,...
          (setf c1 (car c_list))                        ; Set c1
          (setf c_list (cdr c_list))                    ; Point to next element
          ;; First set up the left operand, X1
          (setf X1 (car literals))                      ; X1 = the first literal
          (if (eql X1 node)                             ; Is it the node with new solutions?
              (setf X1-solutions new-solutions)         ; If so, process new-solutions *only*
            (setf X1-solutions (solutions X1)))         ; If not, get the set of old solutions
          (prepare-left-operand c1 X1-solutions)        ; Prepare X1
          ;; For each of the rest of siblings, X2:
          (dolist (X2 (cdr literals))
            ;; If some silbing has null TV, this means case 2 is broken
            (if (null (solutions X2))
              (return-from propagate))
            ;; break out of loop if "head" literal is reached
            (if (equal (node-type X2) #\H)
              (return))
            (if (eql X2 node)                           ; Is it the node with new solutions?
                (setf X2-solutions new-solutions)       ; If so, process new-solutions *only*
              (setf X2-solutions (solutions X2)))       ; If not, get the set of old solutions
            (setf c2 (car c_list))                      ; Set c2
            (setf c_list (cdr c_list))                  ; Point to next element
            ;; Merge with sibling's sequence:
            (setf X1-solutions
              (merge-solutions c2 X1-solutions X2-solutions)))
          ;; Now find a head with solution
              
          ;; If new-solutions is not empty
          (if (not (null X1-solutions))
            (progn
              ;; apply "theta":  result = (1 - theta) * AND-factor + theta * OR-factor
              (apply-theta theta X1-solutions)
              (****DEBUG 1 "propagate: got first TV = ~a" (message (car X1-solutions)))
              ;; send new-solutions to parent node;  recurse
              (propagate parent X1-solutions))))
))))

;;; Merge two lists of solutions
;;; INPUT:   two lists of solutions
;;;          if merging is OK, perform sum-product operation
;;; OUTPUT:  new list of merged solutions
(defun merge-solutions (c2 soln-list1 soln-list2)
  (setf result-solution-list nil)
  ;; Pick an element from list1
  (dolist (solution1 soln-list1)
    ;; Add the following results to the total results
    (setf result-solution-list (nconc result-solution-list
      ;; Try to merge element1 with list2 elements
      (merge-single-solution c2 solution1 soln-list2))))
  ;; return the results
  result-solution-list)

;;; Merge a single solution against a list of solutions
(defun merge-single-solution (c2 solution1 soln-list2)
  (setf soln-list0 nil)
  (dolist (solution2 soln-list2)
    (block outer-loop
      (setf sub1 (sub solution1)
            sub2 (sub solution2))
      ;; Merge sub1 and sub2 to give sub0, avoiding duplicates
      ;;   First add to sub0 elements in sub1 that are not in sub2
      (setf sub0 nil)
      (dolist (pair1 sub1)
        (dolist (pair2 sub2)
          ;; var1 and var2 are different
          (if (not (equal (car pair1) (car pair2)))
            ;; add the pair to the result (sub0)
            (push pair1 sub0)
            ;; var1 and var2 are same:
            ;; sub1 and sub2 NOT equal?
            (if (not (equal (cdr pair1) (cdr pair2)))
              ;; break from the loop
              (return-from outer-loop)
              ;; otherwise omit this pair, do nothing
              ))))
      ;; Then add the remaining elements in sub2 to sub0
      (setf sub0 (nconc sub0 sub2))
      ;; now sub0 has the new merged sub
      ;; calculate:  X0 = sum-product X1 X2
      (setf x0 (sum-product c2 (message solution1) (message solution2)))
      ;; Create solution0 with the new X0
      (setf solution0 (make-instance 'solution
                              :sub     sub0
                              :message x0))
      ;; add solution0 to soln-list0
      (push solution0 soln-list0)))
      ;; return the new list
      soln-list0)

;;; Prepare leftmost operand to be used in sum-product calulation
;;; INPUT:   list of solutions, each solution has a message equal to its TV = x1
;;; OUTPUT:  each message in each solution is a triple:  (1 . (2 . 3))
;;;  where   1 = x1  (unchanged)
;;;          2 = AND-factor = c1 * x1 + ~c1 * ~x1
;;;          3 = OR-factor  = same as AND-factor
(defun prepare-left-operand (c1 soln-list)
  (dolist (soln soln-list)
    (if (numberp (message soln))
      (let* ((x1         (message soln))
             (AND-factor (+ (*      c1       x1)
                            (* (- 1 c1) (- 1 x1)))))
        (setf (message soln) `(,x1 ,AND-factor . ,AND-factor))))))

;;; Calculate the sum-product message required by the factor graph algorithm
;;; INPUT:   c2 msg1 msg2
;;;          theta = number in [0,1]
;;;          msg1  = the triple: (X1 . (AND-factor . OR-factor))
;;;          msg2  = TV of X2
;;; OUTPUT:  AND-factor = SUM[ c1 * c2 * x1 * x2 ]
;;;          OR-factor  = SUM[ (c1 + c2 - c1 * c2) * x1 * x2 ]
;;;          where each c1, c2, x1, x2 must be negated once during the sum, ie, ~c1 = (1 - c1)
;;; Note:  The algorithm below is the "sequential" version;  see the book for an explanation.
(defun sum-product (c2 msg1 msg2)
  (let* ((x1          (car msg1))          ; x1 has been prepared so it must be a triple
         (~x1         (- 1 x1))
         (x2          (if (numberp msg2)   ; x2 may be a number or a triple
                        msg2
                        (car msg2)))
         (~x2         (- 1 x2))
         (~c2         (- 1 c2))
         (AND-f1      (cadr msg1))
         (OR-f1       (cddr msg1)))
    ;; return the triple as CONS cells
    `(,(* x1 x2)                           ; 1st component = product of x1 x2
      ,(* AND-f1 (+ (*  c2  x2)            ; 2nd component = AND-factor
                    (* ~c2 ~x2)))
      .
      ,(+ OR-f1                            ; 3rd component = OR-factor
          (* (+ (*  c2   x2)
                (* ~c2  ~x2))
             (- 1 OR-f1))))))

;;; Apply "theta" to a list of solutions
;;; INPUT:   solution list
;;;          each solution has an AND-factor and OR-factor stored in the message as a CONS cell
;;; OUTPUT:  for each solution, its new message value is set to:
;;;              (1 - theta) * AND-factor + theta * OR-factor
(defun apply-theta (theta soln-list)
  (dolist (soln soln-list)
    (if (consp (message soln))
      (setf (message soln) (+ (* (- 1 theta) (cadr (message soln)))
                              (*      theta  (cddr (message soln))))))))

;;; ********************************* misc functions for printing ******************************

(defun print-priority-list ()
  (****DEBUG 1 "Priority list:")
  (dolist (item priority-list)
    (if (listp (list-data item))
      (****DEBUG 1 "    <~a,~a> ~a" (score item) (depth item) (list-data item))
      (****DEBUG 1 "    <~a,~a> ~a <- ~a" (score item) (depth item)
                                        (head (list-data item)) (body (list-data item))))))

(defun print-proof-tree ()
  (****DEBUG 1 "Proof tree:")
  (print-tree-node "" proof-tree)
  (****DEBUG 1 "^^^^^^^^^^^^^^^^^^^^"))

(defun print-tree-node (spaces node)
  (if (eql node 'fail)
    (****DEBUG 1 "~a_fact: <fail>" spaces)
    (if (null node)
      (****DEBUG 1 "~a_nil" spaces)
      (let ((data (node-data node)))
        ;; null?
        (if (null data)
          (****DEBUG 1 "~a_nil <~a>" spaces (print-solutions (solutions node)))
        ;; else if... sub-goal or fact?
        (if (listp data)
          ;; fact?
          (if (eql 'fact (car data))
            (****DEBUG 1 "~a_fact: 'fact <~a>" spaces (print-solutions (solutions node)))
            ;; subgoal:
            (progn
              (****DEBUG 1 "~a_sub-goal: head = ~a <~a>" spaces (car data) (print-solutions (solutions node)))
              (dolist (item (cdr data))      ; cdr skips the subgoal and gets to the literals
                (print-tree-node (concatenate 'string spaces "___") item))))
          ;; rule?
          (progn
            (setf op (car (parameters data)))
            (****DEBUG 1 "~a_rule: op = ~a <~a>" spaces op (print-solutions (solutions node)))
            (dolist (arg (literals data))
              (if (floatp arg)
                (return))
              (print-tree-node (concatenate 'string spaces "___") arg)))))))))

(defun print-explanation (tree)
   (if (not (null tree))
     (progn
       (if (null (third tree))
         ;; 1-ary operator
         (progn
           (format t "~a~%" (car tree))
           (if (eql 'tree-node (type-of (second tree)))
             (print-explanation2 (node-data (second tree)))
             (print-explanation (second tree))))
         ;; 2-ary operator
         (progn
           (if (eql 'tree-node (type-of (second tree)))
             (print-explanation2 (node-data (second tree)))
             (print-explanation (second tree)))
           (format t " ~a~%" (car tree))
           (if (eql 'tree-node (type-of (third tree)))
             (print-explanation2 (node-data (third tree)))
             (print-explanation (third  tree))))))))

(defun print-explanation2 (data)
  (if (eql 'fact (car data))
    (format t "~a" (cadr data))
    (format t "~a" (car data))))

;;; Print the list of solutions into a string
;;;
;;; The escape sequences below set colors.  For a list of ASCII color codes see:
;;;     http://en.wikipedia.org/wiki/ANSI_escape_code#Sequence_elements
;;; -------------------------------------------------------------------
;;; |   0   |  1  |   2   |    3    |   4   |    5    |   6   |   7    |
;;; | Black | Red | Green | Yellow  | Blue  | Magenta | Cyan  | White  |
;;; --------------------------------------------------------------- ---
;;; 30-37 = text color
;;; 40-47 = background color

(if (equal (lisp-implementation-type) "CLISP")
  (progn                               ; for CLISP
    (defparameter *Esc* #\Esc)
    (defparameter *fail-message*    (format nil "~C[36mfail~C[0m" *Esc* *Esc*))
    (defparameter *no-soln-message* (format nil "~C[36mno solution~C[0m" *Esc* *Esc*))
    )
  (progn                               ; for ABCL, but ABCL can't print ASCII colors anyway
    (defparameter *Esc* #\Escape)
    (defparameter *fail-message*    "fail")
    (defparameter *no-soln-message* "no solution")  
    ))

(defun print-solutions (solutions)
  (if (equal 'fail solutions)
    (return-from print-solutions *fail-message*))
  (if (null solutions)
    (return-from print-solutions *no-soln-message*))
  (setf str " ")
  (dolist (solution solutions)
    (setf str
      (concatenate 'string str
           (if (equal (lisp-implementation-type) "CLISP")
             (format nil "~C[36m~a~C[0m " *Esc* (sub solution) *Esc*)    ; print in color
             (format nil "~a " (sub solution)))))
    (setf str
      (concatenate 'string str
           (if (equal (lisp-implementation-type) "CLISP")
             (format nil "~C[31m~a~C[0m " *Esc* (message solution) *Esc*)     ; print in color
             (format nil "~a " (message solution)))))
  )
  str)              ; return the string