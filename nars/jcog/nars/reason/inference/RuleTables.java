/*
 * RuleTables.java
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
package jcog.nars.reason.inference;

import opencog.atom.rdf.RDFStatement;

import org.opencog.atom.nars.Concept;
import org.opencog.atom.nars.Goal;
import org.opencog.atom.nars.Judgment;
import org.opencog.atom.nars.Sentence;
import org.opencog.atom.nars.Task;
import org.opencog.atom.nars.TaskLink;
import org.opencog.atom.nars.TermLink;
import org.opencog.reason.nars.Memory;
import org.opencog.reason.nars.language.CompoundTerm;
import org.opencog.reason.nars.language.Conjunction;
import org.opencog.reason.nars.language.Disjunction;
import org.opencog.reason.nars.language.Equivalence;
import org.opencog.reason.nars.language.Implication;
import org.opencog.reason.nars.language.Inheritance;
import org.opencog.reason.nars.language.Negation;
import org.opencog.reason.nars.language.SetExt;
import org.opencog.reason.nars.language.SetInt;
import org.opencog.reason.nars.language.Similarity;
import org.opencog.reason.nars.language.Term;
import org.opencog.reason.nars.language.Variable;

/**
 * Table of inference rules, indexed by the TermLinks for the task and the belief.
 * Used in indirective processing of a task, to dispatch inference cases to the
 * relevant inference rules.
 */
public final class RuleTables {

    /**
     * Entry point of the inference engine
     * @param tLink The selected TaskLink, which will provide a task
     * @param bLink The selected TermLink, which may provide a belief
     */
    public static void reason(TaskLink tLink, TermLink bLink) {
        Task task = Memory.currentTask;
        Term taskTerm = (Term) task.getContent().clone();         // cloning for substitution
        Term beliefTerm = (Term) bLink.getTarget().clone();       // cloning for substitution
        Concept beliefConcept = Memory.termToConcept(beliefTerm);
        Judgment belief = null;
        if (beliefConcept != null) {
            belief = beliefConcept.getBelief(task);
        }
        Memory.currentBelief = belief;  // may be null
        if ((belief != null) && (Variable.findSubstitute(Variable.VarType.QUERY, taskTerm, beliefTerm) != null)) {
            MatchingRules.match(task, belief);
        }
        short tIndex = tLink.getIndex(0);
        short bIndex = bLink.getIndex(0);
        switch (tLink.getType()) {          // dispatch first by TaskLink type
            case TermLink.SELF:
                switch (bLink.getType()) {
                    case TermLink.COMPONENT:
                        if ((task.getSentence() instanceof Goal) && Conjunction.isSequence(taskTerm) && (bIndex > 0)) { // more general solution?
                            return;
                        }
                        compoundAndSelf((CompoundTerm) taskTerm, beliefTerm, true);
                        break;
                    case TermLink.COMPOUND:
                        compoundAndSelf((CompoundTerm) beliefTerm, taskTerm, false);
                        break;
                    case TermLink.COMPONENT_STATEMENT:
                        if (belief != null) {
                            SyllogisticRules.detachment(task.getSentence(), belief, bIndex);
                        }
                        break;
                    case TermLink.COMPOUND_STATEMENT:
                        if (belief != null) {
                            SyllogisticRules.detachment(belief, task.getSentence(), bIndex);
                        }
                        break;
                    case TermLink.COMPONENT_CONDITION:
                        if (belief != null) {
                            bIndex = bLink.getIndex(1);
                            SyllogisticRules.conditionalDedInd((Implication) taskTerm, bIndex, beliefTerm, tIndex);
                        }
                        break;
                    case TermLink.COMPOUND_CONDITION:
                        if (belief != null) {
                            bIndex = bLink.getIndex(1);
                            SyllogisticRules.conditionalDedInd((Implication) beliefTerm, bIndex, taskTerm, tIndex);
                        }
                        break;
                }
                break;
            case TermLink.COMPOUND:
                switch (bLink.getType()) {
                    case TermLink.COMPOUND:
                        compoundAndCompound((CompoundTerm) taskTerm, (CompoundTerm) beliefTerm);
                        break;
                    case TermLink.COMPOUND_STATEMENT:
                        compoundAndStatement((CompoundTerm) taskTerm, tIndex, (RDFStatement) beliefTerm, bIndex, beliefTerm);
                        break;
                    case TermLink.COMPOUND_CONDITION:
                        if (belief != null) {
                            SyllogisticRules.conditionalDedInd((Implication) beliefTerm, bIndex, taskTerm, -1);
                        }
                        break;
                }
                break;
            case TermLink.COMPOUND_STATEMENT:
                switch (bLink.getType()) {
                    case TermLink.COMPONENT:
                        componentAndStatement((CompoundTerm) Memory.currentTerm, bIndex, (RDFStatement) taskTerm, tIndex);
                        break;
                    case TermLink.COMPOUND:
                        compoundAndStatement((CompoundTerm) beliefTerm, bIndex, (RDFStatement) taskTerm, tIndex, beliefTerm);
                        break;
                    case TermLink.COMPOUND_STATEMENT:
                        if (belief != null) {
                            bIndex = bLink.getIndex(1);
                            syllogisms(tLink, bLink, taskTerm, beliefTerm);
                        }
                        break;
                    case TermLink.COMPOUND_CONDITION:
                        if (belief != null) {
                            bIndex = bLink.getIndex(1);
                            conditionalDedIndWithVar((Implication) beliefTerm, bIndex, (RDFStatement) taskTerm, tIndex);
                        }
                        break;
                }
                break;
            case TermLink.COMPOUND_CONDITION:
                switch (bLink.getType()) {
                    case TermLink.COMPOUND_STATEMENT:
                        if (belief != null) {
                            conditionalDedIndWithVar((Implication) taskTerm, tIndex, (RDFStatement) beliefTerm, bIndex);
                        }
                        break;
                }
                break;
        }
    }

    /* ----- syllogistic inferences ----- */
    /**
     * Meta-table of syllogistic rules, indexed by the content classes of the 
     * taskSentence and the belief
     * @param tLink The link to task
     * @param bLink The link to belief
     * @param taskTerm The content of task
     * @param beliefTerm The content of belief
     */
    private static void syllogisms(TaskLink tLink, TermLink bLink, Term taskTerm, Term beliefTerm) {
        Sentence taskSentence = Memory.currentTask.getSentence();
        Judgment belief = Memory.currentBelief;
        int figure;
        if (taskTerm instanceof Inheritance) {
            if (beliefTerm instanceof Inheritance) {
                figure = indexToFigure(tLink, bLink);
                asymmetricAsymmetric(taskSentence, belief, figure);
            } else if (beliefTerm instanceof Similarity) {
                figure = indexToFigure(tLink, bLink);
                asymmetricSymmetric(taskSentence, belief, figure);
            } else {
                detachmentWithVar(belief, taskSentence, bLink.getIndex(0));
            }
        } else if (taskTerm instanceof Similarity) {
            if (beliefTerm instanceof Inheritance) {
                figure = indexToFigure(bLink, tLink);
                asymmetricSymmetric(belief, taskSentence, figure);
            } else if (beliefTerm instanceof Similarity) {
                figure = indexToFigure(bLink, tLink);
                symmetricSymmetric(belief, taskSentence, figure);
            }
        } else if (taskTerm instanceof Implication) {
            if (beliefTerm instanceof Implication) {
                figure = indexToFigure(tLink, bLink);
                asymmetricAsymmetric(taskSentence, belief, figure);
            } else if (beliefTerm instanceof Equivalence) {
                figure = indexToFigure(tLink, bLink);
                asymmetricSymmetric(taskSentence, belief, figure);
            } else if (beliefTerm instanceof Inheritance) {
                detachmentWithVar(taskSentence, belief, tLink.getIndex(0));
            }
        } else if (taskTerm instanceof Equivalence) {
            if (beliefTerm instanceof Implication) {
                figure = indexToFigure(bLink, tLink);
                asymmetricSymmetric(belief, taskSentence, figure);
            } else if (beliefTerm instanceof Equivalence) {
                figure = indexToFigure(bLink, tLink);
                symmetricSymmetric(belief, taskSentence, figure);
            } else if (beliefTerm instanceof Inheritance) {
                detachmentWithVar(taskSentence, belief, tLink.getIndex(0));
            }
        }
    }

    /**
     * Decide the figure of syllogism according to the locations of the common 
     * term in the premises
     * @param link1 The link to the first premise
     * @param link2 The link to the second premise
     * @return The figure of the syllogism, one of the four: 11, 12, 21, or 22
     */
    private static int indexToFigure(TermLink link1, TermLink link2) {
        return (link1.getIndex(0) + 1) * 10 + (link2.getIndex(0) + 1);
    }

    /**
     * Syllogistic rules whose both premises are on the same asymmetric relation
     * @param sentence The taskSentence in the task
     * @param belief The judgment in the belief
     * @param figure The location of the shared term
     */
    private static void asymmetricAsymmetric(Sentence sentence, Judgment belief, int figure) {
        RDFStatement s1 = (RDFStatement) sentence.cloneContent();
        RDFStatement s2 = (RDFStatement) belief.cloneContent();
        Term t1, t2;
        switch (figure) {
            case 11:    // induction
                if (Variable.unify(Variable.VarType.INDEPENDENT, s1.getSubject(), s2.getSubject(), s1, s2)) {
                    t1 = s2.getPredicate();
                    t2 = s1.getPredicate();
                    SyllogisticRules.abdIndCom(t1, t2, sentence, belief, figure);
                    CompositionalRules.composeCompound(sentence, belief, 0);
                }
                break;
            case 12:    // deduction
                if (Variable.unify(Variable.VarType.INDEPENDENT, s1.getSubject(), s2.getPredicate(), s1, s2)) {
                    t1 = s2.getSubject();
                    t2 = s1.getPredicate();
                    if (Variable.unify(Variable.VarType.QUERY, t1, t2, s1, s2)) {
                        MatchingRules.matchReverse();
                    } else {
                        SyllogisticRules.dedExe(t1, t2, sentence, belief);
                    }
                }
                break;
            case 21:    // exemplification
                if (Variable.unify(Variable.VarType.INDEPENDENT, s1.getPredicate(), s2.getSubject(), s1, s2)) {
                    t1 = s1.getSubject();
                    t2 = s2.getPredicate();
                    if (Variable.unify(Variable.VarType.QUERY, t1, t2, s1, s2)) {
                        MatchingRules.matchReverse();
                    } else {
                        SyllogisticRules.dedExe(t1, t2, sentence, belief);
                    }
                }
                break;
            case 22:    // abduction
                if (Variable.unify(Variable.VarType.INDEPENDENT, s1.getPredicate(), s2.getPredicate(), s1, s2)) {
                    t1 = s1.getSubject();
                    t2 = s2.getSubject();
                    if (!SyllogisticRules.conditionalAbd(t1, t2, s1, s2)) {         // if conditional abduction, skip the following
                        SyllogisticRules.abdIndCom(t1, t2, sentence, belief, figure);
                        CompositionalRules.composeCompound(sentence, belief, 1);
                    }
                }
                break;
            default:
        }
    }

    /**
     * Syllogistic rules whose first premise is on an asymmetric relation, and the 
     * second on a symmetric relation
     * @param asym The asymmetric premise
     * @param sym The symmetric premise
     * @param figure The location of the shared term
     */
    private static void asymmetricSymmetric(Sentence asym, Sentence sym, int figure) {
        if (asym.getTense() == sym.getTense()) {
            Memory.currentTense = asym.getTense();
        } else {
            return;
        }
        RDFStatement asymSt = (RDFStatement) asym.cloneContent();
        RDFStatement symSt = (RDFStatement) sym.cloneContent();
        Term t1, t2;
        switch (figure) {
            case 11:
                if (Variable.unify(Variable.VarType.INDEPENDENT, asymSt.getSubject(), symSt.getSubject(), asymSt, symSt)) {
                    t1 = asymSt.getPredicate();
                    t2 = symSt.getPredicate();
                    if (Variable.unify(Variable.VarType.QUERY, t1, t2, asymSt, symSt)) {
                        MatchingRules.matchAsymSym(asym, sym, figure);
                    } else {
                        SyllogisticRules.analogy(t2, t1, asym, sym, figure);
                    }
                }
                break;
            case 12:
                if (Variable.unify(Variable.VarType.INDEPENDENT, asymSt.getSubject(), symSt.getPredicate(), asymSt, symSt)) {
                    t1 = asymSt.getPredicate();
                    t2 = symSt.getSubject();
                    if (Variable.unify(Variable.VarType.QUERY, t1, t2, asymSt, symSt)) {
                        MatchingRules.matchAsymSym(asym, sym, figure);
                    } else {
                        SyllogisticRules.analogy(t2, t1, asym, sym, figure);
                    }
                }
                break;
            case 21:
                if (Variable.unify(Variable.VarType.INDEPENDENT, asymSt.getPredicate(), symSt.getSubject(), asymSt, symSt)) {
                    t1 = asymSt.getSubject();
                    t2 = symSt.getPredicate();
                    if (Variable.unify(Variable.VarType.QUERY, t1, t2, asymSt, symSt)) {
                        MatchingRules.matchAsymSym(asym, sym, figure);
                    } else {
                        SyllogisticRules.analogy(t1, t2, asym, sym, figure);
                    }
                }
                break;
            case 22:
                if (Variable.unify(Variable.VarType.INDEPENDENT, asymSt.getPredicate(), symSt.getPredicate(), asymSt, symSt)) {
                    t1 = asymSt.getSubject();
                    t2 = symSt.getSubject();
                    if (Variable.unify(Variable.VarType.QUERY, t1, t2, asymSt, symSt)) {
                        MatchingRules.matchAsymSym(asym, sym, figure);
                    } else {
                        SyllogisticRules.analogy(t1, t2, asym, sym, figure);
                    }
                }
                break;
        }
    }

    /**
     * Syllogistic rules whose both premises are on the same symmetric relation
     * @param belief The premise that comes from a belief
     * @param taskSentence The premise that comes from a task
     * @param figure The location of the shared term
     */
    private static void symmetricSymmetric(Judgment belief, Sentence taskSentence, int figure) {
        if (taskSentence.getTense() == belief.getTense()) {
            Memory.currentTense = taskSentence.getTense();
        } else {
            return;
        }
        RDFStatement s1 = (RDFStatement) belief.cloneContent();
        RDFStatement s2 = (RDFStatement) taskSentence.cloneContent();
        switch (figure) {
            case 11:
                if (Variable.unify(Variable.VarType.INDEPENDENT, s1.getSubject(), s2.getSubject(), s1, s2)) {
                    SyllogisticRules.resemblance(s1.getPredicate(), s2.getPredicate(), belief, taskSentence, figure);
                }
                break;
            case 12:
                if (Variable.unify(Variable.VarType.INDEPENDENT, s1.getSubject(), s2.getPredicate(), s1, s2)) {
                    SyllogisticRules.resemblance(s1.getPredicate(), s2.getSubject(), belief, taskSentence, figure);
                }
                break;
            case 21:
                if (Variable.unify(Variable.VarType.INDEPENDENT, s1.getPredicate(), s2.getSubject(), s1, s2)) {
                    SyllogisticRules.resemblance(s1.getSubject(), s2.getPredicate(), belief, taskSentence, figure);
                }
                break;
            case 22:
                if (Variable.unify(Variable.VarType.INDEPENDENT, s1.getPredicate(), s2.getPredicate(), s1, s2)) {
                    SyllogisticRules.resemblance(s1.getSubject(), s2.getSubject(), belief, taskSentence, figure);
                }
                break;
        }
    }

    /* ----- conditional inferences ----- */
    /**
     * The detachment rule, with variable unification
     * @param mainSentence The premise that is an Implication or Equivalence
     * @param subSentence The premise that is the subject or predicate of the first one
     * @param index The location of the second premise in the first
     */
    private static void detachmentWithVar(Sentence mainSentence, Sentence subSentence, int index) {
        RDFStatement statement = (RDFStatement) mainSentence.getContent();
        Term component = statement.componentAt(index);
        Term content = subSentence.getContent();
        if ((component instanceof Inheritance) && (Memory.currentBelief != null)) {
            if (component.isConstant()) {
                SyllogisticRules.detachment(mainSentence, subSentence, index);
            } else if (Variable.unify(Variable.VarType.INDEPENDENT, component, content, statement, content)) {
                SyllogisticRules.detachment(mainSentence, subSentence, index);
            } else if ((statement instanceof Implication) && (Memory.currentTask.getSentence().isJudgment())) {
                RDFStatement s2 = (RDFStatement) statement.getPredicate();
                if (s2.getSubject().equals(((RDFStatement) content).getSubject())) {
                    SyllogisticRules.introVarIndInner((RDFStatement) content, s2, statement);    // tense???
                    CompositionalRules.introVarDepInner(statement, s2, content);  // tense???
                }
            }
        }
    }

    /**
     * Conditional deduction or induction, with variable unification
     * @param conditional The premise that is an Implication with a Conjunction as condition
     * @param index The location of the shared term in the condition
     * @param statement The second premise that is a statement
     * @param side The location of the shared term in the statement
     */
    private static void conditionalDedIndWithVar(Implication conditional, short index, RDFStatement statement, short side) {
        CompoundTerm condition = (CompoundTerm) conditional.getSubject();
        Term component = condition.componentAt(index);
        Term component2 = null;
        if (statement instanceof Inheritance) {
            component2 = statement;
            side = -1;
        } else if (statement instanceof Implication) {
            component2 = statement.componentAt(side);
        }
        if ((component2 != null) && Variable.unify(Variable.VarType.ALL, component, component2, conditional, statement)) {
            SyllogisticRules.conditionalDedInd(conditional, index, statement, side);
        }
    }

    /* ----- structural inferences ----- */
    /**
     * Inference between a compound term and a component of it
     * @param compound The compound term
     * @param component The component term
     * @param compoundTask Whether the compound comes from the task
     */
    private static void compoundAndSelf(CompoundTerm compound, Term component, boolean compoundTask) {
        if ((compound instanceof Conjunction) || (compound instanceof Disjunction)) {
            if (Memory.currentBelief != null) {
                CompositionalRules.decomposeStatement(compound, component, compoundTask);
            } else if (compound.containComponent(component)) {
                StructuralRules.structuralCompound(compound, component, compoundTask);
            }
        } else if ((compound instanceof Negation) && !Memory.currentTask.isStructual()) {
            if (compoundTask) {
                StructuralRules.transformNegation(((Negation) compound).componentAt(0));
            } else {
                StructuralRules.transformNegation(compound);
            }
        }
    }

    /**
     * Inference between two compound terms
     * @param taskTerm The compound from the task
     * @param beliefTerm The compound from the belief
     */
    private static void compoundAndCompound(CompoundTerm taskTerm, CompoundTerm beliefTerm) {
        if (taskTerm.getClass() == beliefTerm.getClass()) {
            if (taskTerm.size() > beliefTerm.size()) {
                compoundAndSelf(taskTerm, beliefTerm, true);
            } else if (taskTerm.size() < beliefTerm.size()) {
                compoundAndSelf(beliefTerm, taskTerm, false);
            }
        }
    }

    /**
     * Inference between a compound term and a statement
     * @param compound The compound term
     * @param index The location of the current term in the compound
     * @param statement The statement
     * @param side The location of the current term in the statement
     * @param beliefTerm The content of the belief
     */
    private static void compoundAndStatement(CompoundTerm compound, short index, RDFStatement statement, short side, Term beliefTerm) {
        Term component = compound.componentAt(index);
        Task task = Memory.currentTask;
        if (component.getClass() == statement.getClass()) {
            if ((compound instanceof Conjunction) && (Memory.currentBelief != null)) {
                if (Variable.unify(Variable.VarType.DEPENDENT, component, statement, compound, statement)) {
                    CompositionalRules.abdVarDepOuter(compound, component, statement.equals(beliefTerm));
                } else if (task.getSentence().isJudgment()) {
                    SyllogisticRules.introVarIndInner(statement, (RDFStatement) component, compound);
                    CompositionalRules.introVarDepInner(compound, component, statement);
                }
            }
        } else {
            if (!task.isStructual() && task.getSentence().isJudgment()) {
                if (statement instanceof Inheritance) {
                    StructuralRules.structuralCompose1(compound, index, statement);
                    if (!(compound instanceof SetExt) && !(compound instanceof SetInt)) {
                        StructuralRules.structuralCompose2(compound, index, statement, side);
                    }    // {A --> B, A @ (A&C)} |- (A&C) --> (B&C)
                } else if (statement instanceof Similarity) {
                    StructuralRules.structuralCompose2(compound, index, statement, side);
                }       // {A <-> B, A @ (A&C)} |- (A&C) <-> (B&C)
            }
        }
    }

    /**
     * Inference between a component term (of the current term) and a statement
     * @param compound The compound term
     * @param index The location of the current term in the compound
     * @param statement The statement
     * @param side The location of the current term in the statement
     */
    private static void componentAndStatement(CompoundTerm compound, short index, RDFStatement statement, short side) {
        if (!Memory.currentTask.isStructual()) {
            if (statement instanceof Inheritance) {
                StructuralRules.structuralDecompose1(compound, index, statement);
                if (!(compound instanceof SetExt) && !(compound instanceof SetInt)) {
                    StructuralRules.structuralDecompose2(statement);    // {(C-B) --> (C-A), A @ (C-A)} |- A --> B
                } else {
                    StructuralRules.transformSetRelation(compound, statement, side);
                }
            } else if (statement instanceof Similarity) {
                StructuralRules.structuralDecompose2(statement);        // {(C-B) --> (C-A), A @ (C-A)} |- A --> B
                if ((compound instanceof SetExt) || (compound instanceof SetInt)) {
                    StructuralRules.transformSetRelation(compound, statement, side);
                }
            } else if ((statement instanceof Implication) && (compound instanceof Negation)) {
                StructuralRules.contraposition(statement);
            }
        }
    }

    /* ----- inference with one TaskLink only ----- */
    /**
     * The TaskLink is of type TRANSFORM, and the conclusion is an equivalent transformation
     * @param task The task
     * @param tLink The task link
     */
    public static void transformTask(Task task, TaskLink tLink) {
        CompoundTerm content = (CompoundTerm) task.getContent().clone();
        short[] indices = tLink.getIndices();
        Term inh = null;
        if (indices.length == 2) {          // <(*, term, #) --> #>
            inh = content;
        } else if (indices.length == 3) {   // <<(*, term, #) --> #> ==> #>
            inh = content.componentAt(indices[0]);
        } else if (indices.length == 4) {   // <(&&, <(*, term, #) --> #>, #) ==> #>
            if ((indices[0] == 0) && (content instanceof Implication) && (content.componentAt(0) instanceof Conjunction)) {
                inh = ((CompoundTerm) content.componentAt(0)).componentAt(indices[1]);
            } else {
                return;
            }
        }
        if (inh instanceof Inheritance) {
            StructuralRules.transformProductImage((Inheritance) inh, content, indices, task);
        }
    }
}
