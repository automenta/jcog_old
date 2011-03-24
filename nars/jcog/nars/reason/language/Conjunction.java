/*
 * Conjunction.java
 *
 * Copyright (C) 2008  Pei Wang
 *
 * This file is part of Open-NARS.
 *
 * Open-NARS is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * Open-NARS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Open-NARS.  If not, see <http://www.gnu.org/licenses/>.
 */
package jcog.nars.reason.language;

import java.util.ArrayList;
import java.util.TreeSet;

import org.opencog.atom.nars.TemporalValue;
import org.opencog.reason.nars.Memory;
import org.opencog.reason.nars.io.Symbols;

/**
 * Conjunction of statements
 */
public class Conjunction extends CompoundTerm {

    /**
     * Temporal order between the components
     */
    private TemporalValue temporalOrder = null;

    /**
     * Constructor with partial values, called by make
     * @param n The name of the term
     * @param arg The component list of the term
     * @param order The temporal order of the components
     */
    protected Conjunction(String n, ArrayList<Term> arg, TemporalValue order) {
        super(n, arg);
        temporalOrder = order;
    }

    /**
     * Constructor with full values, called by clone
     * @param n The name of the term
     * @param cs Component list
     * @param open Open variable list
     * @param i Syntactic complexity of the compound
     * @param order The temporal order of the components
     */
    protected Conjunction(String n, ArrayList<Term> cs, ArrayList<Variable> open, short i, TemporalValue order) {
        super(n, cs, open, i);
        temporalOrder = order;
    }

    /**
     * Clone an object
     * @return A new object
     */
    @SuppressWarnings("unchecked")
    public Object clone() {
        return new Conjunction(name, (ArrayList<Term>) cloneList(components), (ArrayList<Variable>) cloneList(openVariables), complexity, temporalOrder);
    }

    /**
     * Get the operator of the term.
     * @return the operator of the term
     */
    public String operator() {
        return getConjunctionSymbol(temporalOrder);
    }

    /**
     * COnvert a temporal into its String representation
     * @param t The given temporal value
     * @return String representation of the order
     */
    public static String getConjunctionSymbol(TemporalValue t) {
        if (t == null)
            return Symbols.CONJUNCTION_OPERATOR;
        int delta = t.getDelta();
        if (delta > 0)
            return Symbols.SEQUENCE_OPERATOR;
        if (delta < 0)
            return "ERROR: UNKNOWN CONJUNCTION";
        return Symbols.PARALLEL_OPERATOR;
    }

    /**
     * Check if the compound is communitative.
     * @return true for communitative
     */
    @Override
    public boolean isCommutative() {
        return ((temporalOrder == null) || (temporalOrder.getDelta() == 0));
    }


    /**
     * Return the temporal order.
     * @return Temporal order of the components
     */
    @Override
    public TemporalValue getOrder() {
        return temporalOrder;
    }

    /**
     * Whether the Term is a parallel conjunction
     * @param t A given term
     * @return If the term is a paralel conjunction
     */
    public static boolean isParallel(Term t) {
        if (t instanceof Conjunction) {
            Conjunction c = (Conjunction) t;
            TemporalValue temp = c.getOrder();
            if (temp != null && (temp.getDelta() == 0)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Whether the Term is a sequential conjunction
     * @param t A given term
     * @return If the term is a sequential conjunction
     */
    public static boolean isSequence(Term t) {
        if (t instanceof Conjunction) {
            Conjunction c = (Conjunction) t;
            TemporalValue temp = c.getOrder();
            if (temp != null && (temp.getDelta() == 1)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Try to make a new compound from a list of components. Called by StringParser.
     * @return the Term generated from the arguments
     * @param argList the list of arguments
     * @param order The temporal order of the components
     */
    public static Term make(Memory memory, ArrayList<Term> argList, TemporalValue order) {
        if ((order != null) && (order.getDelta() > 0)) {
            String name = makeCompoundName(Symbols.SEQUENCE_OPERATOR, argList);
            Term t = memory.nameToListedTerm(name);
            return (t != null) ? t : new Conjunction(name, argList, order);            
        } else {
            TreeSet<Term> set = new TreeSet<Term>(argList); // sort/merge arguments
            return make(memory, set, order);
        }
    }

    /**
     * Try to make a new compound from a set of components. Called by the public make methods.
     * @param set a set of Term as compoments
     * @param order The temporal order of the components
     * @return the Term generated from the arguments
     */
    public static Term make(Memory memory, TreeSet<Term> set, TemporalValue order) {
        if (set.isEmpty()) {
            return null;
        }                         // special case: no component
        if (set.size() == 1) {
            return set.first();
        }                         // special case: single component
        ArrayList<Term> argument = new ArrayList<Term>(set);
        String sym = getConjunctionSymbol(order);
        String name = makeCompoundName(sym, argument);
        Term t = memory.nameToListedTerm(name);
        return (t != null) ? t : new Conjunction(name, argument, order);
    }

    // overload this method by term type?
    /**
     * Try to make a new compound from two components. Called by the inference rules.
     * @param term1 The first compoment
     * @param term2 The second compoment
     * @param order The temporal order of the components
     * @return A compound generated or a term it reduced to
     */
    @SuppressWarnings("unchecked")
    public static Term make(Memory memory, Term term1, Term term2, TemporalValue order) {
        if ((order != null) && order.getDelta() > 0) { 
            ArrayList<Term> argument;
            if (isSequence(term2)) { // to be refined to check other cases
                argument = ((CompoundTerm) term2).cloneComponents();
                argument.add(0, term1);
            } else {
                argument = new ArrayList<Term>();
                argument.add(term1);
                argument.add(term2);
            }
            return make(memory, argument, order);          
        } else { // to be refined to check other cases
            TreeSet set;
            if ((order != null) && (order.getDelta() == 0)) {
                if (isParallel(term1)) {
                    set = new TreeSet(((CompoundTerm) term1).cloneComponents());
                    if (isParallel(term2)) {
                        set.addAll(((CompoundTerm) term2).cloneComponents());
                    } else {
                        set.add((Term) term2.clone());
                    }                          // (&,(&,P,Q),R) = (&,P,Q,R)
                } else if (isParallel(term2)) {
                    set = new TreeSet(((CompoundTerm) term2).cloneComponents());
                    set.add((Term) term1.clone());   // (&,R,(&,P,Q)) = (&,P,Q,R)
                } else {
                    set = new TreeSet();
                    set.add(term1); 
                    set.add(term2);
                    return make(memory, set, order);
                }
            } else { // if (order == null)
                if (term1 instanceof Conjunction) {
                    set = new TreeSet(((CompoundTerm) term1).cloneComponents());
                    if (term2 instanceof Conjunction) {
                        set.addAll(((CompoundTerm) term2).cloneComponents());
                    } // (&,(&,P,Q),(&,R,S)) = (&,P,Q,R,S)
                    else {
                        set.add((Term) term2.clone());
                    }                          // (&,(&,P,Q),R) = (&,P,Q,R)
                } else if (term2 instanceof Conjunction) {
                    set = new TreeSet(((CompoundTerm) term2).cloneComponents());
                    set.add((Term) term1.clone());  // (&,R,(&,P,Q)) = (&,P,Q,R)
                } else {
                    set = new TreeSet();
                    set.add(term1);         // clone?
                    set.add(term2);         // clone?
                }
                return make(memory, set, order);
            }
        }
        return null; // report error? how about compound sets?
    }
}
