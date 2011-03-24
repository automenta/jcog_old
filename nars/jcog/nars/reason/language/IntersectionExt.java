/*
 * IntersectionExt.java
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

import nars.io.Symbols;
import nars.main.Memory;

/**
 * A compound term whose extension is the intersection of the extensions of its components
 */
public class IntersectionExt extends CompoundTerm {

    /**
     * Constructor with partial values, called by make
     * @param n The name of the term
     * @param arg The component list of the term
     */
    private IntersectionExt(String n, ArrayList<Term> arg) {
        super(n, arg);
    }

    /**
     * Constructor with full values, called by clone
     * @param n The name of the term
     * @param cs Component list
     * @param open Open variable list
     * @param i Syntactic complexity of the compound
     */
    private IntersectionExt(String n, ArrayList<Term> cs, ArrayList<Variable> open, short i) {
        super(n, cs, open, i);
    }

    /**
     * Clone an object
     * @return A new object, to be casted into a IntersectionExt
     */
    @SuppressWarnings("unchecked")
    public Object clone() {
        return new IntersectionExt(name, (ArrayList<Term>) cloneList(components), (ArrayList<Variable>) cloneList(openVariables), complexity);
    }

    /**
     * Try to make a new compound from two components. Called by the inference rules.
     * @param term1 The first compoment
     * @param term2 The first compoment
     * @return A compound generated or a term it reduced to
     */
    @SuppressWarnings("unchecked")
    public static Term make(Term term1, Term term2) {
        TreeSet set;
        if ((term1 instanceof SetInt) && (term2 instanceof SetInt)) {
            set = new TreeSet(((CompoundTerm) term1).cloneComponents());
            set.addAll(((CompoundTerm) term2).cloneComponents());        // set union
            return SetInt.make(set);
        }
        if ((term1 instanceof SetExt) && (term2 instanceof SetExt)) {
            set = new TreeSet(((CompoundTerm) term1).cloneComponents());
            set.retainAll(((CompoundTerm) term2).cloneComponents());     // set intersection
            return SetExt.make(set);
        }
        if (term1 instanceof IntersectionExt) {
            set = new TreeSet(((CompoundTerm) term1).cloneComponents());
            if (term2 instanceof IntersectionExt) {
                set.addAll(((CompoundTerm) term2).cloneComponents());
            }               // (&,(&,P,Q),(&,R,S)) = (&,P,Q,R,S)
            else {
                set.add((Term) term2.clone());
            }               // (&,(&,P,Q),R) = (&,P,Q,R)
        } else if (term2 instanceof IntersectionExt) {
            set = new TreeSet(((CompoundTerm) term2).cloneComponents());
            set.add((Term) term1.clone());    // (&,R,(&,P,Q)) = (&,P,Q,R)
        } else {
            set = new TreeSet();
            set.add((Term) term1.clone());
            set.add((Term) term2.clone());
        }
        return make(set);
    }

    /**
     * Try to make a new IntersectionExt. Called by StringParser.
     * @return the Term generated from the arguments
     * @param argList The list of components
     */
    public static Term make(ArrayList<Term> argList) {
        TreeSet<Term> set = new TreeSet<Term>(argList); // sort/merge arguments
        return make(set);
    }

    /**
     * Try to make a new compound from a set of components. Called by the public make methods.
     * @param set a set of Term as compoments
     * @return the Term generated from the arguments
     */
    public static Term make(TreeSet<Term> set) {
        if (set.size() == 1) {
            return set.first();
        }                         // special case: single component
        ArrayList<Term> argument = new ArrayList<Term>(set);
        String name = makeCompoundName(Symbols.INTERSECTION_EXT_OPERATOR, argument);
        Term t = Memory.nameToListedTerm(name);
        return (t != null) ? t : new IntersectionExt(name, argument);
    }

    /**
     * Get the operator of the term.
     * @return the operator of the term
     */
    public String operator() {
        return Symbols.INTERSECTION_EXT_OPERATOR;
    }

    /**
     * Check if the compound is communitative.
     * @return true for communitative
     */
    @Override
    public boolean isCommutative() {
        return true;
    }
}
