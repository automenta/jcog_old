/*
 * Equivalence.java
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

import org.opencog.atom.nars.TemporalValue;
import org.opencog.reason.nars.Memory;
import org.opencog.reason.nars.io.Symbols;

/**
 * A Statement about an Equivalence relation.
 */
public class Equivalence extends Statement {

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
    protected Equivalence(String n, ArrayList<Term> arg, TemporalValue order) {
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
    protected Equivalence(String n, ArrayList<Term> cs, ArrayList<Variable> open, short i, TemporalValue order) {
        super(n, cs, open, i);
        temporalOrder = order;
    }

    /**
     * Clone an object
     * @return A new object
     */
    @SuppressWarnings("unchecked")
    public Object clone() {
        return new Equivalence(name, (ArrayList<Term>) cloneList(components), (ArrayList<Variable>) cloneList(openVariables), complexity, temporalOrder);
    }

    /**
     * Try to make a new compound from two components. Called by the inference rules.
     * @param subject The first compoment
     * @param predicate The second compoment
     * @param order The temporal order of the components
     * @return A compound generated or null
     */
    public static Equivalence make(Memory memory, Term subject, Term predicate, TemporalValue order) {  // to be extended to check if subject is Conjunction
        if ((subject instanceof Implication) || (subject instanceof Equivalence)) {
            return null;
        }
        if ((predicate instanceof Implication) || (predicate instanceof Equivalence)) {
            return null;
        }
        if (invalidStatement(subject, predicate)) {
            return null;
        }
        Term interm;
        if ((subject.compareTo(predicate) > 0) && ((order == null) || (order.getDelta() == 0))) {
            interm = subject;
            subject = predicate;
            predicate = interm;
        } else if ((order != null) && (order.getDelta() < 0)) {
            interm = subject;
            subject = predicate;
            predicate = interm;
            order = TemporalValue.getReverse(order);
        }
        String sym = getEquivalenceSymbol(order);
        String name = makeStatementName(subject, sym, predicate);
        Term t = memory.nameToListedTerm(name);
        if (t != null) {
            return (Equivalence) t;
        }
        ArrayList<Term> argument = argumentsToList(subject, predicate);
        return new Equivalence(name, argument, order);
    }

    /**
     * Get the operator of the term.
     * @return the operator of the term
     */
    public String operator() {
        return getEquivalenceSymbol(temporalOrder);
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
     * Get the temporal order.
     * @return the temporal order of the components
     */
    @Override
    public TemporalValue getOrder() {
        return temporalOrder;
    }

    /**
     * Get the symbole of the relation by tense
     * @param t The tense of the statement
     * @return The String representation of the relation
     */
    public static String getEquivalenceSymbol(TemporalValue t) {
        if (t == null)
            return Symbols.EQUIVALENCE_RELATION;
        int delta = t.getDelta();
        if (delta > 0)
            return Symbols.EQUIVALENCE_AFTER_RELATION;
        if (delta < 0)
            return "ERROR: UNKNOWN EQUIVALENCE";
        return Symbols.EQUIVALENCE_WHEN_RELATION;
    }
}
