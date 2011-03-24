/*
 * Implication.java
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
 * A Statement about an Inheritance relation.
 */
public class Implication extends Statement {
    
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
    protected Implication(String n, ArrayList<Term> arg, TemporalValue order) {
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
    protected Implication(String n, ArrayList<Term> cs, ArrayList<Variable> open, short i, TemporalValue order) {
        super(n, cs, open, i);
        temporalOrder = order;
    }

    /**
     * Clone an object
     * @return A new object
     */
    @SuppressWarnings("unchecked")
    public Object clone() {
        return new Implication(name, (ArrayList<Term>) cloneList(components), (ArrayList<Variable>) cloneList(openVariables), complexity, temporalOrder);
    }

    /**
     * Try to make a new compound from two components. Called by the inference rules.
     * @param subject The first compoment
     * @param predicate The second compoment
     * @param order The temporal order of the components
     * @return A compound generated or a term it reduced to
     */
    public static Implication make(Memory memory, Term subject, Term predicate, TemporalValue order) {
        if ((subject instanceof Implication) || (subject instanceof Equivalence) || (predicate instanceof Equivalence))
            return null;
        if (invalidStatement(subject, predicate))
            return null;
        String sym = getSymbol(order);
        String name = makeStatementName(subject, sym, predicate);
        Term t = memory.nameToListedTerm(name);
        if (t != null) {
            return (Implication) t;
        }
        if (predicate instanceof Implication) {
            Term oldCondition = ((Implication) predicate).getSubject();
            Term newCondition = Conjunction.make(memory, subject, oldCondition, order);
            return make(memory, newCondition, ((Implication) predicate).getPredicate(), order);
        } else {
            ArrayList<Term> argument = argumentsToList(subject, predicate);
            return new Implication(name, argument, order);
        }
    }

    /**
     * Get the operator of the term.
     * @return the operator of the term
     */
    public String operator() {
        return getSymbol(temporalOrder);
    }
        
    
    /**
     * Get the order of the components.
     * @return the order within the term
     */
    @Override
    public TemporalValue getOrder() {
        return temporalOrder;
    }

    /**
     * Convert a TemporalValue into its String representation
     * @param t The temporal value to be represented
     * @return The the String representation
     */
    public static String getSymbol(TemporalValue t) {
        if (t == null)
            return Symbols.IMPLICATION_RELATION;
        int delta = t.getDelta();
        if (delta > 0)
            return Symbols.IMPLICATION_AFTER_RELATION;
        if (delta < 0)
            return Symbols.IMPLICATION_BEFORE_RELATION;
        return Symbols.IMPLICATION_WHEN_RELATION;
    }
}
