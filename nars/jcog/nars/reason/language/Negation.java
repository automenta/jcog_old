/*
 * Negation.java
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

import java.util.*;
import jcog.nars.reason.Memory;
import jcog.nars.reason.io.Symbols;


/**
 * A negation of a Statement.
 */
public class Negation extends CompoundTerm {

    /**
     * Constructor with partial values, called by make
     * @param n The name of the term
     * @param arg The component list of the term
     */
    private Negation(String n, ArrayList<Term> arg) {
        super(n, arg);
    }

    /**
     * Constructor with full values, called by clone
     * @param n The name of the term
     * @param cs Component list
     * @param open Open variable list
     * @param i Syntactic complexity of the compound
     */
    private Negation(String n, ArrayList<Term> cs, ArrayList<Variable> open, short i) {
        super(n, cs, open, i);
    }

    /**
     * Clone an object
     * @return A new object
     */
    @SuppressWarnings("unchecked")
    public Object clone() {
        return new Negation(name, (ArrayList<Term>) cloneList(components), (ArrayList<Variable>) cloneList(openVariables), complexity);
    }

    /**
     * Try to make a Negation of one component. Called by the inference rules.
     * @param t The compoment
     * @return A compound generated or a term it reduced to
     */
    public static Term make(Memory m, Term t) {
        if (t instanceof Negation) {
            return ((CompoundTerm) t).cloneComponents().get(0);
        }         // (--,(--,P)) = P
        ArrayList<Term> argument = new ArrayList<Term>();
        argument.add(t);
        return make(m, argument);
    }

    /**
     * Try to make a new SetExt. Called by StringParser.
     * @return the Term generated from the arguments
     * @param argument The list of components
     */
    public static Term make(Memory m, ArrayList<Term> argument) {
        if (argument.size() != 1) {
            return null;
        }
        String name = makeCompoundName(Symbols.NEGATION_OPERATOR, argument);
        Term t = m.nameToListedTerm(name);
        return (t != null) ? t : new Negation(name, argument);
    }

    /**
     * Get the operator of the term.
     * @return the operator of the term
     */
    public String operator() {
        return Symbols.NEGATION_OPERATOR;
    }
}
