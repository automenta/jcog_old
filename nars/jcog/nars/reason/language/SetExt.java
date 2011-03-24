/*
 * SetExt.java
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

import org.opencog.reason.nars.Memory;
import org.opencog.reason.nars.io.Symbols;



/**
 * An extensionally defined set, which contains one or more instances.
 */
public class SetExt extends CompoundTerm {

    /**
     * Constructor with partial values, called by make
     * @param n The name of the term
     * @param arg The component list of the term
     */
    private SetExt(String n, ArrayList<Term> arg) {
        super(n, arg);
    }

    /**
     * Constructor with full values, called by clone
     * @param n The name of the term
     * @param cs Component list
     * @param open Open variable list
     * @param i Syntactic complexity of the compound
     */
    private SetExt(String n, ArrayList<Term> cs, ArrayList<Variable> open, short i) {
        super(n, cs, open, i);
    }

    /**
     * Clone a SetExt
     * @return A new object, to be casted into a SetExt
     */
    @SuppressWarnings("unchecked")
    public Object clone() {
        return new SetExt(name, (ArrayList<Term>) cloneList(components), (ArrayList<Variable>) cloneList(openVariables), complexity);
    }

    /**
     * Try to make a new set from one component. Called by the inference rules.
     * @param t The compoment
     * @return A compound generated or a term it reduced to
     */
    public static Term make(Term t) {
        TreeSet<Term> set = new TreeSet<Term>();
        set.add(t);
        return make(set);
    }

    /**
     * Try to make a new SetExt. Called by StringParser.
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
        if (set.isEmpty()) {
            return null;
        }
        ArrayList<Term> argument = new ArrayList<Term>(set);
        String name = makeSetName(Symbols.SET_EXT_OPENER, argument, Symbols.SET_EXT_CLOSER);
        Term t = Memory.nameToListedTerm(name);
        return (t != null) ? t : new SetExt(name, argument);
    }

    /**
     * Get the operator of the term.
     * @return the operator of the term
     */
    public String operator() {
        return "" + Symbols.SET_EXT_OPENER;
    }

    /**
     * Check if the compound is communitative.
     * @return true for communitative
     */
    @Override
    public boolean isCommutative() {
        return true;
    }

    /**
     * Make a String representation of the set, override the default.
     * @return true for communitative
     */
    @Override
    public String makeName() {
        return makeSetName(Symbols.SET_EXT_OPENER, components, Symbols.SET_EXT_CLOSER);
    }
}

