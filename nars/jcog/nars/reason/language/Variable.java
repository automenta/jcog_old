/*
 * Variable.java
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
import java.util.HashMap;
import jcog.nars.reason.io.Symbols;


/**
 * A variable term, which does not correspond to a concept
 */
public class Variable extends Term {

    /**
     * Four variable types plus a wild card
     */
    public enum VarType {

        INDEPENDENT, DEPENDENT, ANONYMOUS, QUERY, ALL
    }
    /** Type of the variable */
    private VarType type;
    /** The minimum CompoundTerm what contains all occurences of this variable */
    private CompoundTerm scope;

    /**
     * Constructor, from a given variable name
     * @param s A String read from input
     */
    public Variable(String s) {
        name = s;
        scope = null;
        char prefix = s.charAt(0);
        if (prefix == Symbols.QUERY_VARIABLE_TAG) {
            type = VarType.QUERY;
        } else if (s.length() == 1) {
            type = VarType.ANONYMOUS;
        } else if (s.charAt(s.length() - 1) == Symbols.COMPOUND_TERM_CLOSER) {
            type = VarType.DEPENDENT;
            int i = s.indexOf(Symbols.COMPOUND_TERM_OPENER);
            name = s.substring(0, i);    // name does not include the dependency list
        } else {
            type = VarType.INDEPENDENT;
        }
    }

    /**
     * Constructor, with all fields
     * @param n Variable name
     * @param s Variable scope
     * @param t Variable type
     */
    private Variable(String n, VarType t, CompoundTerm s) {
        name = n;
        type = t;
        scope = s;
    }

    /**
     * Clone a Variable
     * @return The cloned Variable
     */
    @Override
    public Object clone() {
        return new Variable(name, type, scope);
    }

    /**
     * Variable name is omitted in sorting
     * @return The Variable prefix
     */
    @Override
    public String getConstantName() {
        return name.substring(0, 1);
    }

    /**
     * Rename a variable temporally to distinguish it from variables in other Terms
     * @param first Whether it is the first term
     * @return The new name
     */
    public String getVarName(boolean first) {
        if (first) {
            return Symbols.VARIABLE_TAG + "1" + name;
        } else {
            return Symbols.VARIABLE_TAG + "2" + name;
        }
    }

    /**
     * Rename the variable
     * @param n The new name
     */
    public void setName(String n) {
        name = n;
    }

    /**
     * Get the type of the variable
     * @return The variable type
     */
    public VarType getType() {
        return type;
    }
    
    /**
     * Set the scope of the variable
     * @param t The new scope
     */
    public void setScope (CompoundTerm t) {
        scope = t;
    }

    /**
     * Whether this variable equals another one
     * @param that The other variable
     * @return Whether the two are equal
     */
    @Override
    public boolean equals(Object that) {
        return (that instanceof Variable) && name.equals(((Variable) that).getStoredName());
    }

    /**
     * Get a hash code for the variable
     * @return The hash code
     */
    @Override
    public int hashCode() {
        int hash = 7;
        return hash;
    }

    /**
     * Get variable name without dependency list (if any)
     * @return The stored name
     */
    public String getStoredName() {
        return name;
    }

    /**
     * Get variable name with dependency list (if any)
     * @return The generated name
     */
    @Override
    public String getName() {
        if (type != VarType.DEPENDENT) {
            return name;
        } else {
            StringBuffer buffer = new StringBuffer(name + "(");
            if (scope != null) {
                ArrayList<Variable> dependency = scope.getOpenVariables();
                if (dependency != null) {
                    for (Variable v : dependency) {
                        if (v.getType() == VarType.INDEPENDENT) {
                            buffer.append(v.toString());
                        }
                    }
                }
            }
            buffer.append(")");
            return buffer.toString();
        }
    }

    /**
     * A variable is constant
     * @return false
     */
    @Override
    public boolean isConstant() {
        return false;
    }

    /**
     * Whether two variable types can match each other in unification
     * @param type1 Type of the first variable
     * @param type2 Type of the second variable
     * @return The matching result
     */
    public static boolean match(VarType type1, VarType type2) {
        return ((type1 == type2) 
                || (type1 == VarType.ALL) || (type1 == VarType.QUERY) || (type1 == VarType.ANONYMOUS) 
                || (type2 == VarType.ALL) || (type2 == VarType.QUERY) || (type2 == VarType.ANONYMOUS));
    }

    /**
     * To unify two Terms, then apply the substitution to the two compounds
     * @param type The type of Variable to be unified
     * @param t1 The first Term to be unified
     * @param t2 The second Term to be unified
     * @param compound1 The first compound to be substituted
     * @param compound2 The second compound to be substituted
     * @return Whether a unification has been succeeded
     */
    public static boolean unify(VarType type, Term t1, Term t2, Term compound1, Term compound2) {
        if (t1.isConstant() && t1.equals(t2)) {// to constant Terms are unified if equals
            return true;
        }
        if (!(compound1 instanceof CompoundTerm) || !(compound2 instanceof CompoundTerm)) {
            return false;
        }
        HashMap<String, Term> substitute = findSubstitute(type, t1, t2, new HashMap<String, Term>()); // find substitution
        if (substitute == null) // not unifiable
        {
            return false;
        }
        if (!substitute.isEmpty()) {
            ((CompoundTerm) compound1).substituteComponent(substitute, true);   // apply the substitution to the first compound
            ((CompoundTerm) compound2).substituteComponent(substitute, false);  // apply the substitution to the second compound
        }
        return true;
    }

    /**
     * To find a substitution that can unify two Terms without changing them
     * @param type The type of Variable to be substituted
     * @param term1 The first Term to be unified
     * @param term2 The second Term to be unified
     * @return The substitution that unifies the two Terms
     */
    public static HashMap<String, Term> findSubstitute(VarType type, Term term1, Term term2) {
        return findSubstitute(type, term1, term2, new HashMap<String, Term>());
    }

    /**
     * To recursively find a substitution that can unify two Terms without changing them
     * @param type The type of Variable to be substituted
     * @param term1 The first Term to be unified
     * @param term2 The second Term to be unified
     * @param subs The substitution formed so far
     * @return The substitution that unifies the two Terms
     */
    private static HashMap<String, Term> findSubstitute(VarType type, Term term1, Term term2, HashMap<String, Term> subs) {
        Term t1, t2;
        if (term1.equals(term2)) {// for constant, also shortcut for variable and compound
            return subs;
        }
        if ((term1 instanceof Variable) && match(((Variable) term1).getType(), type)) { // the first Term is a unifiable Variable
            return findSubstituteVar(type, (Variable) term1, term2, subs, true);
        }
        if ((term2 instanceof Variable) && match(((Variable) term2).getType(), type)) {
            return findSubstituteVar(type, (Variable) term2, term1, subs, false);
        }
        if ((term1 instanceof Variable) && (term2 instanceof Variable) && match(((Variable) term1).getType(), ((Variable) term2).getType())) {
            return findSubstituteVar(((Variable) term1).getType(), (Variable) term1, term2, subs, true);
        }
        if (term1 instanceof CompoundTerm) {
            if (!term1.getClass().equals(term2.getClass())) {
                return null;
            }
            if (!(((CompoundTerm) term1).size() == ((CompoundTerm) term2).size())) {
                return null;
            }
            for (int i = 0; i < ((CompoundTerm) term1).size(); i++) {
                t1 = ((CompoundTerm) term1).componentAt(i);
                t2 = ((CompoundTerm) term2).componentAt(i);
                HashMap<String, Term> newSubs = findSubstitute(type, t1, t2, subs);
                if (newSubs == null) {
                    return null;
                }
                subs.putAll(newSubs);
            }
            return subs;
        }
        return null;
    }

    /**
     * To find a substitution that can unify a Vriable and a Term
     * @param type The type of Variable to be substituted
     * @param var The Variable to be unified
     * @param term The Term to be unified
     * @param subs The substitution formed so far
     * @param first If it is the first Term in unify
     * @return The substitution that unifies the two Terms, as "name-term" pairs
     */
    private static HashMap<String, Term> findSubstituteVar(VarType type, Variable var, Term term, HashMap<String, Term> subs, boolean first) {
        String name1 = var.getVarName(first);
        Term oldTerm = subs.get(name1);
        if (oldTerm != null) {
            if (first) {
                return findSubstitute(type, oldTerm, term, subs);
            } else {
                return findSubstitute(type, term, oldTerm, subs);
            }
        } else {
            if (term instanceof Variable) {
                String name2 = ((Variable) term).getVarName(!first);
                oldTerm = subs.get(name2);
                if (oldTerm != null) {
                    if (first) {
                        return findSubstitute(type, var, oldTerm, subs);
                    } else {
                        return findSubstitute(type, oldTerm, var, subs);
                    }
                }
            }
            subs.put(name1, term);
            return subs;
        }
    }
}
