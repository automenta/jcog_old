;;;; Genifer/deduction.lisp
;;;;
;;;; Copyright (C) 2009 Genint
;;;; All Rights Reserved
;;;;
;;;; Written by Abram Demski, YKY
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
;;;; **** Deduction, backward-chaining, using Z logic, best-first search

;Format for combinator trees: The functions s and k are defined and function-application is represented as usual in Lisp; this is intuitive but a bit misleading since combonator trees will not normally be executed using regular lisp evaluation. Because of the notation I chose, though, one can do such a thing with combinator trees (if they happen to be in flattened form *and* have exactly the right number of arguments for each instance of the combinators). Note also that there is no assumption made that the combinator trees are pure; the code should work with other expressions mixed in.

;All functions are non-destructive.

;Note that the following two definitions are totally unnecessary unless one *does* try to use s and k as normal lisp functions.

(defmacro s (&rest args) (loop-exec (cons 's args)))
(defmacro k (&rest args) (loop-exec (cons 'k args)))
(defmacro c (&rest args) (loop-exec (cons 'c args)))
(defmacro e (&rest args) (loop-exec (cons 'c args)))

;Now on to serious matters.

(defun args-needed (tree)
  "If either s or k are in the head position, returns the number of additional arguments needed to evaluate them (may be 0). Returns nil otherwise."
  (cond
    ((null tree) nil)
    ((eq tree 's) 3)
    ((eq tree 'k) 2)
    ((atom tree) nil)
    (t
      (let
        (
         (head-value (args-needed (car tree))) )
        (if head-value (max (- head-value (length (cdr tree))) 0)) ) ) ) )

(defun flatten (tree)
  "Several Lisp trees can represent the same combinator tree, thanks to currying of arguments. For example, ((s k) k) is really the same as (s k k). This takes a tree and returns a version of it with as few parentheses as possible, without executing anything."
  (cond
    ((null tree) tree)
    ((atom tree) tree)
    ((equal (length tree) 1) (flatten (car tree)))
    ((atom (car tree)) (mapcar 'flatten tree))
    ((listp (car tree)) (append (flatten (car tree)) (mapcar 'flatten (cdr tree)))) ) )

(defun exec (tree)
  "Given a tree, executes the combinator in the head position if there are enough arguments, returning the result. Otherwise, it just returns the tree."
  (if (eq (args-needed tree) 0)
    (let
      ((flat-tree (flatten tree)))
      (if (eq (car tree) 's)
        (cons
            ;; S x y z = (x z) (y z)
            `((,(second flat-tree) ,(fourth flat-tree))
                 (,(third flat-tree) ,(fourth flat-tree)))
            ;; args from #5 onwards
            (cddddr flat-tree) )
        (cons
            ;; K x y = x
            (second flat-tree)
            ;; args from #4 onwards
            (cdddr flat-tree)) ) )
    tree ) )

(defun loop-exec (tree)
  "Executes the head location for as long as it is possible to do so."
  (loop with tr = tree while (eq (args-needed tr) 0) do (setq tr (exec tr)) finally (return (flatten tr))) )

(defun s-reversible (tree)
  "Determines if an s-application can be reversed in the head location."
  (let ((flat-tree (flatten tree)))
    (equal (car (cdr flat-tree)) (car (cdr (car (cdr (cdr flat-tree)))))) ) )

;Note: things are always k-reversible in any location, so we don't need a test. However, k destroys information when it executes, so reversing it is not deterministic like s!

(defun s-reverse (tree)
  "Performs s-reversion at the head location, if possible."
  (let ((flat-tree (flatten tree)))
    (if (s-reversible tree)
      `(s ,(car flat-tree) ,(caar (cddr flat-tree)) ,(cadr flat-tree) ,@(cdddr flat-tree))
      flat-tree ) ) )

(defun k-reverse (tree filler)
  "Performs k-reversion at the head location, putting "filler" into the destroyed spot."
  (let ((flat-tree (flatten tree)))
    `(k (car flat-tree) ,filler ,@(cdr flat-tree)) ) )

;Now we could technically make a universal compressor by searching for sequences of backwards moves which result in a smaller tree. I won't for now. :)

;Note also that I have not bothered to write execution or reverse execution methods which look in places other than the head location, because technically it is not needed for completeness.

(defun combinator-equality (tree1 tree2)
  "Tests for equality of two combinator expressions. So long as the two combinator terms halt on every input, correctness of this test is assured; if the expressions do not always halt, false negatives or non-termination may occur. (This shortcoming is impossible to totally eliminate, thanks to the halting problem; anything that always told us when two functions were equal would allow us to simply compare a function to a known-nonhalting function to decide whether it is nonhalting.)"
  (if (equal (flatten tree1) (flatten tree2))
    t
    (let ((t1 tree1) (t2 tree2) (g nil))
      (loop
        (setq t1 (loop-exec t1))
        (setq t2 (loop-exec t2))
        (if (not (args-needed t1)) (return))
        (setq g (gensym))
        (setq t1 (flatten (append (list t1) (list g))))
        (setq t2 (flatten (append (list t2) (list g)))) )
      (cond
        ((atom t1) (equal t1 t2))
        ((atom t2) nil)
        ((not (equal (car t1) (car t2))) nil)
        (t
          (loop
            (if (not (combinator-equality (car t1) (cat t2))) (return nil))
            (pop t1)
            (pop t2)
            (if (and (not t1) (not t2)) (return t))
            (if (or (not t1) (not t2)) (return nil)) ) ) ) ) ) )
