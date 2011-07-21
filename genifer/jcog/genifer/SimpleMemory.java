/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.genifer;

import java.util.LinkedList;
import java.util.List;
import org.apache.commons.collections15.ListUtils;
import org.apache.commons.collections15.Predicate;
import org.armedbear.lisp.*;

/**
 *
 * @author SEH
 */
public class SimpleMemory implements Memory {

    //List<Formula> Formulas = new LinkedList();
    List<Rule> rules = new LinkedList();
    List<Fact> facts = new LinkedList();

    /**
     * 
     * @param tv
     * @param truthOrConfidence true = get truth value, false = get confidence value
     * @return
     */
    protected static double getTV(Cons tv, boolean truthOrConfidence) {
        Object j = (truthOrConfidence) ? tv.car().javaInstance() : tv.cdr().javaInstance();
        if (j instanceof Float) {
            return ((Float) j).floatValue();
        }
        System.err.println("invalid tv: " + tv + " ( " + tv.car() + " , " + tv.cdr().car() + " )");
        return 0.0f;
    }

    public static double getProbability(Cons tv) {
        return getTV(tv, true);
    }

    public static double getConfidence(Cons tv) {
        return getTV(tv, false);
    }

    public static class LispTruth implements Truth {

        private final Cons tv;

        public LispTruth(Cons tv) {
            super();
            this.tv = tv;
        }

        @Override
        public double getConfidence() {
            return SimpleMemory.getConfidence(tv);
        }

        @Override
        public double getProbability() {
            return SimpleMemory.getProbability(tv);
        }

        @Override
        public String toString() {
            return getProbability() + ", " + getConfidence();
        }

    }

    public boolean addFact(Cons formula, Cons tv) {
//        System.out.println(formula.car().javaInstance());
//        System.out.println(tv.car().javaInstance());
        Fact f = new Fact(new Sexp(formula), new LispTruth(tv));
        if (facts.add(f)) {
            //System.out.println("added fact: " + f);
            return true;
        }
        return false;
    }

    public boolean addRule(Cons formula, float w) {
//        System.out.println(formula.car().javaInstance());
//        System.out.println(tv.car().javaInstance());
        Rule r = new Rule(new Sexp(formula), w);
        if (rules.add(r)) {
            //System.out.println("added rule: " + r);
            return true;
        }
        return false;
    }

//    @Override
//    public boolean add(Formula f) {
//        return Formulas.add(f);
//    }
//
//    @Override
//    public boolean remove(Formula f) {
//        return Formulas.remove(f);
//    }
    @Override
    public boolean add(Rule r) {
        return rules.add(r);
    }

    @Override
    public boolean remove(Rule r) {
        return rules.remove(r);
    }

    @Override
    public boolean add(Fact f) {
        return facts.add(f);
    }

    @Override
    public boolean remove(Fact f) {
        return facts.remove(f);
    }

//        @Override
//        public List<Formula> getAllFacts(final String key) {
//            return ListUtils.predicatedList(Formulas, new Predicate<Formula>() {
//                @Override public boolean evaluate(Formula t) {
//                    if (t instanceof Atom) {
//                        return ((Atom)t).name.equals(key);
//                    }
//                     else if (t instanceof Con) {
//
//                     }
//                    return false;
//                }
//            });
//        }
    @Override
    public List<Rule> getAllRules(final String key) {
        return ListUtils.predicatedList(rules, new Predicate<Rule>() {

            @Override public boolean evaluate(Rule r) {
                Formula f = r.formula;
                if (f instanceof Atom) {
                    // TODO:  This is wrong,
                    //        we need the formula's second element here:
                    return ((Atom) r.formula).name.equals(key);
                } else if (f instanceof Sexp) {
                    //_KY_: For rules, we need to match the car of the second element
                    LispObject l = ((Sexp) f).cons.cdr().car();
                    return matchesKey(l, key);
                }
                return false;
            }
        });
    }

    public static boolean matchesKey(LispObject l, String key) {
        if (l.atom()) {
            String s = l.getStringValue();
            System.out.println("matching: " + l.getStringValue() + " w/ " + key);
            if (s.equals(key)) {
                return true;
            }
        }
        return false;
    }

    public List<Fact> getFacts() { return facts; }
    public List<Rule> getRules() { return rules; }

    @Override
    public List<Fact> getAllFacts(final String key) {
        return ListUtils.predicatedList(facts, new Predicate<Fact>() {

            @Override public boolean evaluate(Fact t) {
                Formula f = t.formula;
                if (f instanceof Atom) {
                    return ((Atom) t.formula).name.equals(key);
                } else if (f instanceof Sexp) {
                    //match its car
                    LispObject l = ((Sexp) f).cons.car();
                    return matchesKey(l, key);
                }

                return false;
            }
        });
    }
}
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

