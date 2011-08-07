/*
 * CompoundTerm.java
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
import jcog.nars.TemporalValue;
import jcog.nars.TermLink;
import jcog.nars.reason.Memory;
import jcog.nars.reason.io.Symbols;

/**
 * A CompoundTerm is a Term with internal (syntactic) structure
 * <p>
 * A CompoundTerm consists of a term operator with one or more component Terms.
 * <p>
 * This abstract class contains default methods for all CompoundTerms.
 */
public abstract class CompoundTerm extends Term {

    /** list of (direct) components */
    protected ArrayList<Term> components;
    /** list of open variables in the compound */
    protected ArrayList<Variable> openVariables;
    /** syntactic complexity of the compound, the sum of those of its components plus 1 */
    protected short complexity;

    /**
     * Abstract clone method
     * @return A clone of the compound term
     */
    @Override
    public abstract Object clone();

    /**
     * Abstract method to get the operator of the compound
     * @return The operator in a String
     */
    public abstract String operator();

    /* ----- object builders, called from subclasses ----- */
    /**
     * Default constructor
     */
    protected CompoundTerm() {
    }

    /**
     * Constructor called from subclasses constructors to clone the fields
     * @param n Name 
     * @param cs Component list
     * @param open Open variable list
     * @param i Complexity
     */
    protected CompoundTerm(String n, ArrayList<Term> cs, ArrayList<Variable> open, short i) {
        name = n;
        components = cs;
        openVariables = open;
        complexity = i;
    }

    /**
     * Constructor called from subclasses constructors to initialize the fields
     * @param n Name, to be re-built
     * @param arg Component list
     */
    protected CompoundTerm(String n, ArrayList<Term> arg) {
        components = arg;
        calcComplexity();
        markVariables();
        name = makeName();
    }

    /**
     * The complexity of the term is the sum of those of the components plus 1
     */
    private void calcComplexity() {
        complexity = 1;
        for (Term t : components) {
            complexity += t.getComplexity();
        }
    }

    /**
     * Try to make a compound term from a template and a list of components
     * @param compound The template
     * @param components The components
     * @return A compound term or null
     */
    public static Term make(CompoundTerm compound, ArrayList<Term> components) {
        if (compound instanceof SetExt) {
            return SetExt.make(components);
        }
        if (compound instanceof SetInt) {
            return SetInt.make(components);
        }
        if (compound instanceof IntersectionExt) {
            return IntersectionExt.make(components);
        }
        if (compound instanceof IntersectionInt) {
            return IntersectionInt.make(components);
        }
        if (compound instanceof DifferenceExt) {
            return DifferenceExt.make(components);
        }
        if (compound instanceof DifferenceInt) {
            return DifferenceInt.make(components);
        }
        if (compound instanceof Product) {
            return Product.make(components);
        }
        if (compound instanceof ImageExt) {
            return ImageExt.make(components, ((ImageExt) compound).getRelationIndex());
        }
        if (compound instanceof ImageInt) {
            return ImageInt.make(components, ((ImageInt) compound).getRelationIndex());
        }
        if (compound instanceof Disjunction) {
            return Disjunction.make(components);
        }
        if (compound instanceof Conjunction) {
            return Conjunction.make(components, ((Conjunction) compound).getOrder());
        }
        return null;
    }

    /**
     * Try to make a compound term from an operator and a list of components
     * <p>
     * Called from StringParser
     * @param op Term operator
     * @param arg Component list
     * @return A compound term or null
     */
    public static Term make(Memory m, String op, ArrayList<Term> arg) {
        if (op.length() == 1) {
            if (op.equals(Symbols.INTERSECTION_EXT_OPERATOR)) {
                return IntersectionExt.make(arg);
            }
            if (op.equals(Symbols.INTERSECTION_INT_OPERATOR)) {
                return IntersectionInt.make(arg);
            }
            if (op.equals(Symbols.DIFFERENCE_EXT_OPERATOR)) {
                return DifferenceExt.make(arg);
            }
            if (op.equals(Symbols.DIFFERENCE_INT_OPERATOR)) {
                return DifferenceInt.make(arg);
            }
            if (op.equals(Symbols.PRODUCT_OPERATOR)) {
                return Product.make(arg);
            }
            if (op.equals(Symbols.IMAGE_EXT_OPERATOR)) {
                return ImageExt.make(arg);
            }
            if (op.equals(Symbols.IMAGE_INT_OPERATOR)) {
                return ImageInt.make(arg);
            }
        }
        if (op.length() == 2) {
            if (op.equals(Symbols.NEGATION_OPERATOR)) {
                return Negation.make(m, arg);
            }
            if (op.equals(Symbols.DISJUNCTION_OPERATOR)) {
                return Disjunction.make(arg);
            }
            if (op.equals(Symbols.CONJUNCTION_OPERATOR)) {
                return Conjunction.make(arg, null);
            }
            if (op.equals(Symbols.SEQUENCE_OPERATOR) || op.equals(Symbols.PARALLEL_OPERATOR)) {
                return Conjunction.make(arg, new TemporalValue(op));
            }
        }
        if (isBuiltInOperator(op)) {
            Term sub = Product.make(arg);
            Term pre = Memory.nameToOperator(op);
            return Inheritance.make(sub, pre);
        }
        return null;
    }

    /**
     * Check built-in operator name
     * @param s The String to be checked
     * @return if the given String is an operator name
     */
    private static boolean isBuiltInOperator(String s) {
        assert (s.length() > 2);
        return (s.charAt(0) == Symbols.OPERATOR_TAG) && (Memory.nameToOperator(s) != null);
    }

    /**
     * Check CompoundTerm operator symbol
     * @return if the given String is an operator symbol
     * @param s The String to be checked
     */
    public static boolean isOperator(String s) {
        if (s.length() == 1) {
            return (s.equals(Symbols.INTERSECTION_EXT_OPERATOR) ||
                    s.equals(Symbols.INTERSECTION_INT_OPERATOR) ||
                    s.equals(Symbols.DIFFERENCE_EXT_OPERATOR) ||
                    s.equals(Symbols.DIFFERENCE_INT_OPERATOR) ||
                    s.equals(Symbols.PRODUCT_OPERATOR) ||
                    s.equals(Symbols.IMAGE_EXT_OPERATOR) ||
                    s.equals(Symbols.IMAGE_INT_OPERATOR));
        }
        if (s.length() == 2) {
            return (s.equals(Symbols.NEGATION_OPERATOR) ||
                    s.equals(Symbols.DISJUNCTION_OPERATOR) ||
                    s.equals(Symbols.CONJUNCTION_OPERATOR) ||
                    s.equals(Symbols.SEQUENCE_OPERATOR) ||
                    s.equals(Symbols.PARALLEL_OPERATOR));
        }
        return isBuiltInOperator(s);
    }

    /**
     * build a component list from two terms
     * @param t1 the first component
     * @param t2 the second component
     * @return the component list
     */
    protected static ArrayList<Term> argumentsToList(Term t1, Term t2) {
        ArrayList<Term> list = new ArrayList<Term>(2);
        list.add(t1);
        list.add(t2);
        return list;
    }

    /* ----- utilities for name ----- */
    /**
     * default method to make the name of the current term from existing fields
     * @return the name of the term
     */
    protected String makeName() {
        return makeCompoundName(operator(), components);
    }

    /**
     * default method to make the name of a compound term from given fields
     * @param op the term operator
     * @param arg the list of components
     * @return the name of the term
     */
    protected static String makeCompoundName(String op, ArrayList<Term> arg) {
        StringBuffer name = new StringBuffer();
        name.append(Symbols.COMPOUND_TERM_OPENER);
        name.append(op);
        for (int i = 0; i < arg.size(); i++) {
            name.append(Symbols.ARGUMENT_SEPARATOR);
            name.append(arg.get(i).getName());
        }
        name.append(Symbols.COMPOUND_TERM_CLOSER);
        return name.toString();
    }

    /**
     * make the name of an ExtensionSet or IntensionSet
     * @param opener the set opener
     * @param closer the set closer
     * @param arg the list of components
     * @return the name of the term
     */
    protected static String makeSetName(char opener, ArrayList<Term> arg, char closer) {
        StringBuffer name = new StringBuffer();
        name.append(opener);
        name.append(arg.get(0).toString());
        for (int i = 1; i < arg.size(); i++) {
            name.append(Symbols.ARGUMENT_SEPARATOR);
            name.append(arg.get(i).getName());
        }
        name.append(closer);
        return name.toString();
    }

    /**
     * default method to make the name of an image term from given fields
     * @param op the term operator
     * @param arg the list of components
     * @param relationIndex the location of the place holder
     * @return the name of the term
     */
    protected static String makeImageName(String op, ArrayList<Term> arg, int relationIndex) {
        StringBuffer name = new StringBuffer();
        name.append(Symbols.COMPOUND_TERM_OPENER);
        name.append(op);
        name.append(Symbols.ARGUMENT_SEPARATOR);
        name.append(arg.get(relationIndex).getName());
        for (int i = 0; i < arg.size(); i++) {
            name.append(Symbols.ARGUMENT_SEPARATOR);
            if (i == relationIndex) {
                name.append(Symbols.IMAGE_PLACE_HOLDER);
            } else {
                name.append(arg.get(i).getName());
            }
        }
        name.append(Symbols.COMPOUND_TERM_CLOSER);
        return name.toString();
    }

    /**
     * skip all variable names to produce stable sorting order among components, not for display
     * @return the constant part of the term name
     */
    @Override
    public String getConstantName() {
        StringBuffer s = new StringBuffer();
        s.append(Symbols.COMPOUND_TERM_OPENER);
        s.append(operator());
        s.append(Symbols.ARGUMENT_SEPARATOR);
        for (int i = 0; i < components.size(); i++) {
            s.append(components.get(i).getConstantName());
            s.append(Symbols.ARGUMENT_SEPARATOR);
        }
        s.append(Symbols.COMPOUND_TERM_CLOSER);
        return s.toString();
    }

    /* ----- utilities for other fields ----- */
    /**
     * report the term's syntactic complexity
     * @return the comlexity value
     */
    @Override
    public int getComplexity() {
        return complexity;
    }

    /**
     * check if the term contains free variable
     * @return if the term is a constant
     */
    @Override
    public boolean isConstant() {
        return (openVariables == null);
    }

    /**
     * Check if the order of the components matters
     * <p>
     * commutative CompoundTerms: Sets, Intersections;
     * communative Statements: Similarity, Equivalence, EquivalenceWhen;
     * communative CompoundStatements: Disjunction, Conjunction, ConjunctionParallel
     * @return The default value is false
     */
    public boolean isCommutative() {
        return false;
    }

    /* ----- extend Collection methods to component list ----- */
    /**
     * get the number of components
     * @return the size of the component list
     */
    public int size() {
        return components.size();
    }

    /**
     * get a component by index
     * @param i index of the component
     * @return the component
     */
    public Term componentAt(int i) {
        return components.get(i);
    }

    /**
     * Get the component list
     * @return The component list
     */
    public ArrayList<Term> getComponents() {
        return components;
    }

    /**
     * Clone the component list
     * @return The cloned component list
     */
    public ArrayList<Term> cloneComponents() {
        return cloneList(components);
    }

    /**
     * Deep clone an array list of terms
     * @param original The original component list
     * @return an identical and separate copy of the list
     */
    @SuppressWarnings("unchecked")
    public static ArrayList cloneList(ArrayList original) {
        if (original == null) {
            return null;
        }
        ArrayList arr = new ArrayList(original.size());
        for (int i = 0; i < original.size(); i++) {
            arr.add(((Term) original.get(i)).clone());
        }
        return arr;
    }

    /**
     * Check whether the compound contains a certain component
     * @param t The component to be checked
     * @return Whether the component is in the compound
     */
    public boolean containComponent(Term t) {
        return components.contains(t);
    }

    /**
     * Check whether the compound contains all components of another term, or that term as a whole
     * @param t The other term
     * @return Whether the components are all in the compound
     */
    public boolean containAllComponents(Term t) {
        if (getClass() == t.getClass()) {
            return components.containsAll(((CompoundTerm) t).getComponents());
        } else {
            return components.contains(t);
        }
    }

    /**
     * Try to add a component into a compound
     * @param t1 The compound
     * @param t2 The component
     * @return The new compound
     */
    public static Term addComponents(CompoundTerm t1, Term t2) {
        if (t2 == null) {
            return t1;
        }
        boolean success;
        ArrayList<Term> list = t1.cloneComponents();
        if (t1.getClass() == t2.getClass()) {
            success = list.addAll(((CompoundTerm) t2).getComponents());
        } else {
            success = list.add(t2);
        }
        return (success ? make(t1, list) : null);
    }

    /**
     * Try to remove a component from a compound
     * @param t1 The compound
     * @param t2 The component
     * @return The new compound
     */
    public static Term reduceComponents(CompoundTerm t1, Term t2) {
        boolean success;
        ArrayList<Term> list = t1.cloneComponents();
        if (t1.getClass() == t2.getClass()) {
            success = list.removeAll(((CompoundTerm) t2).getComponents());
        } else {
            success = list.remove(t2);
        }
        return (success ? make(t1, list) : null);
    }

    /**
     * Try to replace a component in a compound at a given index by another one
     * @param compound The compound
     * @param index The location of replacement
     * @param t The new component
     * @return The new compound
     */
    public static Term replaceComponent(CompoundTerm compound, int index, Term t) {
        ArrayList<Term> list = compound.cloneComponents();
        list.remove(index);
        if (t != null) {
            if (compound.getClass() != t.getClass()) {
                list.add(index, t);
            } else {
                ArrayList<Term> list2 = ((CompoundTerm) t).cloneComponents();
                for (int i = 0; i < list2.size(); i++) {
                    list.add(index + i, list2.get(i));
                }
            }
        }
        return make(compound, list);
    }

    /**
     * Try to replace a given component in a compound by another one
     * @param compound The compound
     * @param oldComponent The component to remove
     * @param newComponent The component to add
     * @return The new compound
     */
    public static Term replaceComponent(CompoundTerm compound, Term oldComponent, Term newComponent) {
        ArrayList<Term> list = compound.cloneComponents();
        int index = list.indexOf(oldComponent);
        list.remove(index);
        if (compound.getClass() != newComponent.getClass()) {
            list.add(index, newComponent);
        } else {
            ArrayList<Term> list2 = ((CompoundTerm) newComponent).cloneComponents();
            for (int i = 0; i < list2.size(); i++) {
                list.add(index + i, list2.get(i));
            }
        }
        return make(compound, list);
    }

    /* ----- variable-related utilities ----- */
    /**
     * get the OpenVariables list
     * @return the open variables list
     */
    public ArrayList<Variable> getOpenVariables() {
        return openVariables;
    }

    /**
     * Register open and closed variables in a CompoundTerm
     * <p>
     * An open variable only appears in one components, while a closed variable 
     * appears in multiple components
     * <p>
     * May need further testing and revising on various situations
     */
    private void markVariables() {  // not recursive
        openVariables = new ArrayList<Variable>();
        ArrayList<Variable> closedVariables = new ArrayList<Variable>();
        ArrayList<Variable> list;
        for (Term term : components) {
            if ((term instanceof Variable) && (((Variable) term).getType() != Variable.VarType.ANONYMOUS)) {
                openVariables.add((Variable) term);
            } else if (term instanceof CompoundTerm) {
                list = ((CompoundTerm) term).getOpenVariables();
                if (list != null) {
                    for (Variable var : list) {
                        if (var.getType() == Variable.VarType.QUERY) {
                            openVariables.add(var);
                        } else {
                            int i = openVariables.indexOf(var);
                            if (i >= 0) {
                                var.setScope(this);
                                openVariables.get(i).setScope(this);
                                openVariables.remove(i);
                                closedVariables.add(var);
                            } else {
                                openVariables.add(var);
                            }
                        }
                    }
                }
            }
        }
        if (openVariables.size() == 0) {
            openVariables = null;
        }
    }

    /**
     * Rename variables in input sentence
     * <p>
     * only called from Sentence
     */
    public void renameVariables() {
        renameVariables(new HashMap<String, String>());
    }

    /**
     * Recursively rename variables by their appearing order in the CompoundTerm
     * <p>
     * Since each occurance of a variable is processed exactly once, there will 
     * be no confusion between new names and old names.
     * @param varMap the mapping built so far
     */
    protected void renameVariables(HashMap<String, String> varMap) {
        String oldName, newName;
        for (Term t : components) {
            if ((t instanceof Variable) && (((Variable) t).getType() != Variable.VarType.ANONYMOUS)) {
                oldName = ((Variable) t).getStoredName();
                newName = varMap.get(oldName);
                if (newName == null) {
                    newName = makeVarName(varMap.size(), (Variable) t);
                    varMap.put(oldName, newName);
                }
                ((Variable) t).setName(newName);
            } else if (t instanceof CompoundTerm) {
                ((CompoundTerm) t).renameVariables(varMap);
            }
        }
        name = makeName();
    }

    /**
     * Sequentially generate new variable names
     * @param size The current size of the variable list
     * @param v The variable to be renamed
     * @return A new variable name
     */
    private String makeVarName(int size, Variable v) {
        StringBuffer s = new StringBuffer();
        Variable.VarType type = v.getType();
        if (type == Variable.VarType.QUERY) {
            s.append(Symbols.QUERY_VARIABLE_TAG);
        } else {
            s.append(Symbols.VARIABLE_TAG);
        }
        s.append(size + 1);
        return s.toString();
    }

    /**
     * Substitute a variable component according to a given substitution
     * @param subs The substitution
     * @param first Whether it is the first term in the mapping
     */
    public void substituteComponent(HashMap<String, Term> subs, boolean first) {
        Term t1, t2;
        String varName;
        for (int i = 0; i < size(); i++) {
            t1 = componentAt(i);
            if (t1 instanceof Variable) {
                varName = ((Variable) t1).getVarName(first);
                t2 = subs.get(varName);
                if (t2 != null) {
                    components.set(i, t2);
                }
            } else if (t1 instanceof CompoundTerm) {
                ((CompoundTerm) t1).substituteComponent(subs, first);
            }
        }
        markVariables();
        name = makeName();
    }

    /* ----- link CompoundTerm and its components ----- */
    /**
     * Build TermLink templates to constant components and subcomponents
     * <p>
     * The compound type determines the link type; the component type determines whether to build the link.
     * @return A list of TermLink templates
     */
    public ArrayList<TermLink> prepareComponentLinks() {
        ArrayList<TermLink> componentLinks = new ArrayList<TermLink>();
        short type = (this instanceof Statement) ? TermLink.COMPOUND_STATEMENT : TermLink.COMPOUND;   // default
        prepareComponentLinks(componentLinks, type, this);
        return componentLinks;
    }

    /**
     * Collect TermLink templates into a list, go down one level except in special cases
     * <p>
     * @param componentLinks The list of TermLink templates built so far
     * @param type The type of TermLink to be built
     * @param term The CompoundTerm for which the links are built
     */
    private void prepareComponentLinks(ArrayList<TermLink> componentLinks, short type, CompoundTerm term) {
        Term t1, t2, t3;                    // components at different levels
        for (int i = 0; i < term.size(); i++) {     // first level components
            t1 = term.componentAt(i);
            if (t1.isConstant()) {
                componentLinks.add(new TermLink(t1, type, i));
            }
            if ((this instanceof Implication) && (i == 0) && (t1 instanceof Conjunction)) {
                ((CompoundTerm) t1).prepareComponentLinks(componentLinks, TermLink.COMPOUND_CONDITION, (CompoundTerm) t1);
            } else if (t1 instanceof CompoundTerm) {
                for (int j = 0; j < ((CompoundTerm) t1).size(); j++) {  // second level components
                    t2 = ((CompoundTerm) t1).componentAt(j);
                    if (t2.isConstant()) {
                        if ((t1 instanceof Product) || (t1 instanceof ImageExt) || (t1 instanceof ImageInt)) {
                            if (type == TermLink.COMPOUND_CONDITION) {
                                componentLinks.add(new TermLink(t2, TermLink.TRANSFORM, 0, i, j));
                            } else {
                                componentLinks.add(new TermLink(t2, TermLink.TRANSFORM, i, j));
                            }
                        } else {
                            componentLinks.add(new TermLink(t2, type, i, j));
                        }
                    }
                    if ((t2 instanceof Product) || (t2 instanceof ImageExt) || (t2 instanceof ImageInt)) {
                        for (int k = 0; k < ((CompoundTerm) t2).size(); k++) {
                            t3 = ((CompoundTerm) t2).componentAt(k);
                            if (t3.isConstant()) {                           // third level
                                if (type == TermLink.COMPOUND_CONDITION) {
                                    componentLinks.add(new TermLink(t3, TermLink.TRANSFORM, 0, i, j, k));
                                } else {
                                    componentLinks.add(new TermLink(t3, TermLink.TRANSFORM, i, j, k));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

