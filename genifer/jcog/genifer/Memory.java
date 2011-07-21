/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.genifer;

import java.util.List;

/**
 *
 * @author SEH
 */
public interface Memory {

//    public boolean add(Formula f);
//
//    public boolean remove(Formula f);

    boolean add(Rule r);

    boolean remove(Rule r);
        //Rule addRule(head)
        //Rule addRule(head, body)
        //Rule addRule(head, body, ...)
    
    boolean add(Fact f);

    boolean remove(Fact f);


    public List<Fact> getFacts();
    
    public List<Rule> getRules();

    //List<Formula> getAll(String key);

    List<Rule> getAllRules(String key);    
        //ex: getAllRules("loves") includes (implies (loves john mary) (loves john ann))

    List<Fact> getAllFacts(String key);
        //ex: getAllFacts("loves") includes (loves john mary)     
    
    
    
//;;; **** Add a fact to Generic Memory
//(defun add-fact-to-mem (fact &optional tv justifies justified-by)
//  (if (null tv)
//    (setf tv (cons 1.0 1.0)))          ; default TV
//  (****DEBUG 1 "adding fact to memory: ~a" fact)
//  ;; create an object
//  (setf new-fact (make-instance 'fact-item
//                          :fact         fact
//                          :id           *memory-size*
//                          :tv           tv
//                          :justifies    justifies
//                          :justified-by justified-by))
//  ;; increase the index
//  (incf *memory-size*)
//  (incf *newly-added*)
//  ;; add the object to GM, by appending to the end of list
//  (setf *generic-memory* (cons new-fact *generic-memory*)))
//
//;;; **** Add a rule to Generic Memory
//(defun add-rule-to-mem (head &optional body w e+ e- ancestors ancestor-to)
//  ;; Set default values:
//  (****DEBUG 1 "adding rule to memory: ~a <- ~a" head body)
//  (if (null body) (setf body '(*bodyless*)))
//  (if (null w )   (setf w    100))
//  (if (null e+)   (setf e+   0))
//  (if (null e-)   (setf e-   0))
//  ;; create an object
//  (setf new-rule (make-instance 'rule-item
//                          :head         head
//                          :body         body
//                          :id           *memory-size*
//                          :w            w
//                          :e+           e+
//                          :e-           e-
//                          :ancestors    ancestors
//                          :ancestor-to  ancestor-to))
//  ;; increase the index
//  (incf *memory-size*)
//  (incf *newly-added*)
//  ;; add the object to GM, by appending to the end of list
//  (setf *generic-memory* (cons new-rule *generic-memory*)))
//
//(defun delete-memory-item (item)
//  (setf ptr *generic-memory*)
//  ;; Special case:
//  (if (eql item ptr)
//    (return-from delete-memory-item
//      (setf *generic-memory* (cdr *generic-memory*))))
//  ;; Otherwise, find the item
//  (loop
//    (if (eql item (cdr ptr)) (return))
//    (setf ptr (cdr ptr)))
//  ;; Delete it
//  (setf (cdr ptr) (cdr item)))
//
//;;; Fetch all clauses in KB with the given head-predicate
//;;; Return: a list of rules
//(defun fetch-clauses (head-predicate)
//  (let ((facts-list (list nil))
//        (rules-list (list nil)))
//    (dolist (item *generic-memory*)
//      ;; is it a rule?
//      (if (eql (type-of item) 'rule-item)
//        (let ((head (head item))
//              (body (body item)))
//          ;; Does head of rule match head-predicate?
//          (if (equal (car head) head-predicate)
//            (progn
//              ;; calculate the confidence c from w
//              ;; the function is defined in "PZ-calculus.lisp"
//              (setf confidence (convert-w-2-c (w item)))
//              ;; add it to list-to-be-returned
//              (nconc rules-list (list (make-instance 'clause
//                                        :id         (id item)
//                                        :confidence confidence
//                                        :head       head
//                                        :body       body))))))
//        ;; If it is a fact:
//        ;; Does fact match head-predicate?
//        (if (equal (car (fact item)) head-predicate)
//          (let ((tv (tv item)))
//            ;; add it to list-to-be-returned
//            (nconc facts-list (list (make-instance 'clause
//                                      :id         (id item)
//                                      :confidence (cdr tv)
//                                      :head       (fact item)
//                                      :tv         tv)))))))
//    ;; return the 2 lists, discarding the leading 'nil' items
//    (values (cdr facts-list) (cdr rules-list))))
//
//;;; Comparison predicate for "sort" in function "fetch-clauses"
//;;; should return true iff x1 is strictly less than x2
//;;; if x1 is greater than or equal to x2, return false
//;;; sort seems to order from small to big -- we need to reverse this -- biggest confidence 1st
//;;; each element is a list:  (confidence head body)
//(defun compare-items (x1 x2)
//  (> (car x1) (car x2)))
//
//;;; This function is used in natural-language.lisp
//(defvar *entity-counter* 1)
//;;; **** Creates a new entity
//(defun new-entity ()
//  (incf *entity-counter*))
//
//;;; ------------------------ miscellaneous functions -------------------------
//
//;;; **** Print out memory contents
//(defun dump-memory ()
//  (dolist (item *generic-memory*)
//    (if (eql (type-of item) 'fact-item)
//      ;; print a fact item
//      (progn
//        (format t "**** [~a] fact: ~a ~%" (id item) (fact item))
//        (setf tv (tv item))
//        (format t "  TV:           ~a ~%" (car tv))
//        (format t "  confidence:   ~a ~%" (cdr tv))
//        (format t "  justifies:    ~a ~%" (justifies    item))
//        (format t "  justified-by: ~a ~%" (justified-by item)))
//      ;; print a rule item
//      (progn
//        (format t "**** [~a] rule: ~%"    (id          item))
//        (format t "  head:         ~a ~%" (head        item))
//        (format t "  body:         ~a ~%" (body        item))
//        (format t "  w:            ~a ~%" (w           item))
//        (format t "  e+:           ~a ~%" (e+          item))
//        (format t "  e-:           ~a ~%" (e-          item))
//        (format t "  ancestors:    ~a ~%" (ancestors   item))
//        (format t "  ancestors-to: ~a ~%" (ancestor-to item))))))
}