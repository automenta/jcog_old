/*
 * StructuralRules.java
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

import java.util.ArrayList;

import nars.entity.*;
import nars.language.*;
import nars.main.Memory;

/**
 * Single-premise inference rules involving compound terms.
 * Input are one sentence (the premise) and one TermLink (indicating a component)
 */
public final class StructuralRules {

    /* -------------------- transform between compounds and components -------------------- */
    /**
     * {<S --> P>, S@(S&T)} |- <(S&T) --> (P&T)>
     * {<S --> P>, S@(M-S)} |- <(M-P) --> (M-S)>
     * @param compound The compound term
     * @param index The location of the indicated term in the compound
     * @param statement The premise
     * @param side The location of the indicated term in the premise
     */
    static void structuralCompose2(CompoundTerm compound, short index, RDFStatement statement, short side) {
        if (compound.equals(statement.componentAt(side))) {
            return;
        }
        Term sub = statement.getSubject();
        Term pred = statement.getPredicate();
        ArrayList<Term> components = compound.cloneComponents();
        if (((side == 0) && components.pathContains(pred)) || ((side == 1) && components.pathContains(sub))) {
            return;
        }
        if (side == 0) {
            sub = compound;
            components.set(index, pred);
            pred = CompoundTerm.make(compound, components);
        } else {
            components.set(index, sub);
            sub = CompoundTerm.make(compound, components);
            pred = compound;
        }
        if ((sub == null) || (pred == null)) {
            return;
        }
        Term content;
        if (switchOrder(compound, index)) {
            content = RDFStatement.make(statement, pred, sub);
        } else {
            content = RDFStatement.make(statement, sub, pred);
        }
        if (content == null) {
            return;
        }
        Task task = Memory.currentTask;
        Sentence sentence = task.getSentence();
        TruthValue truth = sentence.getTruth();
        BudgetValue budget;
        if (sentence instanceof Question) {
            budget = BudgetFunctions.compoundBackwardWeak(content);
        } else {
            if (compound.size() > 1) {
                if (sentence.isJudgment()) {
                    truth = TruthFunctions.implying(truth);
                } else {
                    assert (sentence instanceof Goal);
                    truth = TruthFunctions.implied(truth);
                }
            }
            budget = BudgetFunctions.compoundForward(truth, content);
        }
        Memory.singlePremiseTask(budget, content, truth);
    }

    /**
     * {<(S&T) --> (P&T)>, S@(S&T)} |- <S --> P>
     * @param statement The premise
     */
    static void structuralDecompose2(RDFStatement statement) {
        Term subj = statement.getSubject();
        Term pred = statement.getPredicate();
        if (subj.getClass() != pred.getClass()) {
            return;
        }
        CompoundTerm sub = (CompoundTerm) subj;
        CompoundTerm pre = (CompoundTerm) pred;
        if (sub.size() != pre.size()) {
            return;
        }
        int index = -1;
        Term t1, t2;
        for (int i = 0; i < sub.size(); i++) {
            t1 = sub.componentAt(i);
            t2 = pre.componentAt(i);
            if (!t1.equals(t2)) {
                if (index < 0) {
                    index = i;
                } else {
                    return;
                }
            }
        }
        t1 = sub.componentAt(index);
        t2 = pre.componentAt(index);
        Term content;
        if (switchOrder(sub, (short) index)) {
            content = RDFStatement.make(statement, t2, t1);
        } else {
            content = RDFStatement.make(statement, t1, t2);
        }
        if (content == null) {
            return;
        }
        Task task = Memory.currentTask;
        Sentence sentence = task.getSentence();
        TruthValue truth = sentence.getTruth();
        BudgetValue budget;
        if (sentence instanceof Question) {
            budget = BudgetFunctions.compoundBackward(content);
        } else {
            if (sub.size() > 1) {
                if (sentence.isJudgment()) {
                    truth = TruthFunctions.implied(truth);
                } else {
                    assert (sentence instanceof Goal);
                    truth = TruthFunctions.implying(truth);
                }
            }
            budget = BudgetFunctions.compoundForward(truth, content);
        }
        Memory.singlePremiseTask(budget, content, truth);
    }

    /**
     * List the cases where the direction of inheritance is revised in conclusion
     * @param compound The compound term
     * @param index The location of focus in the compound
     * @return Whether the direction of inheritance should be revised
     */
    private static boolean switchOrder(CompoundTerm compound, short index) {
        return ((((compound instanceof DifferenceExt) || (compound instanceof DifferenceInt)) && (index == 1)) ||
                ((compound instanceof ImageExt) && (index != ((ImageExt) compound).getRelationIndex())) ||
                ((compound instanceof ImageInt) && (index != ((ImageInt) compound).getRelationIndex())));
    }

    /**
     * {<S --> P>, P@(P&Q)} |- <S --> (P&Q)>
     * @param compound The compound term
     * @param index The location of the indicated term in the compound
     * @param statement The premise
     */
    static void structuralCompose1(CompoundTerm compound, short index, RDFStatement statement) {
        if (!Memory.currentTask.getSentence().isJudgment()) {
            return;
        }
        Term component = compound.componentAt(index);
        Task task = Memory.currentTask;
        Sentence sentence = task.getSentence();
        TruthValue truth = sentence.getTruth();
        Term subj = statement.getSubject();
        Term pred = statement.getPredicate();
        if (component.equals(subj)) {
            if (compound instanceof IntersectionExt) {
                structuralStatement(compound, pred, TruthFunctions.implying(truth));
            } else if (compound instanceof IntersectionInt) {
                structuralStatement(compound, pred, TruthFunctions.implied(truth));
            } else if ((compound instanceof DifferenceExt) && (index == 0)) {
                structuralStatement(compound, pred, TruthFunctions.implying(truth));
            } else if (compound instanceof DifferenceInt) {
                if (index == 0) {
                    structuralStatement(compound, pred, TruthFunctions.implied(truth));
                } else {
                    structuralStatement(compound, pred, TruthFunctions.negImply(truth));
                }
            }
        } else if (component.equals(pred)) {
            if (compound instanceof IntersectionExt) {
                structuralStatement(subj, compound, TruthFunctions.implied(truth));
            } else if (compound instanceof IntersectionInt) {
                structuralStatement(subj, compound, TruthFunctions.implying(truth));
            } else if (compound instanceof DifferenceExt) {
                if (index == 0) {
                    structuralStatement(subj, compound, TruthFunctions.implied(truth));
                } else {
                    structuralStatement(subj, compound, TruthFunctions.negImply(truth));
                }
            } else if ((compound instanceof DifferenceInt) && (index == 0)) {
                structuralStatement(subj, compound, TruthFunctions.implying(truth));
            }
        }
    }

    /**
     * {<(S&T) --> P>, S@(S&T)} |- <S --> P>
     * @param compound The compound term
     * @param index The location of the indicated term in the compound
     * @param statement The premise
     */
    static void structuralDecompose1(CompoundTerm compound, short index, RDFStatement statement) {
        if (!Memory.currentTask.getSentence().isJudgment()) {
            return;
        }
        Term component = compound.componentAt(index);
        Task task = Memory.currentTask;
        Sentence sentence = task.getSentence();
        TruthValue truth = sentence.getTruth();
        Term subj = statement.getSubject();
        Term pred = statement.getPredicate();
        if (compound.equals(subj)) {
            if (compound instanceof IntersectionExt) {
                structuralStatement(component, pred, TruthFunctions.implied(truth));
            } else if (compound instanceof IntersectionInt) {
                structuralStatement(component, pred, TruthFunctions.implying(truth));
            } else if ((compound instanceof DifferenceExt) && (index == 0)) {
                structuralStatement(component, pred, TruthFunctions.implied(truth));
            } else if (compound instanceof DifferenceInt) {
                if (index == 0) {
                    structuralStatement(component, pred, TruthFunctions.implying(truth));
                } else {
                    structuralStatement(component, pred, TruthFunctions.negImply(truth));
                }
            }
        } else if (compound.equals(pred)) {
            if (compound instanceof IntersectionExt) {
                structuralStatement(subj, component, TruthFunctions.implying(truth));
            } else if (compound instanceof IntersectionInt) {
                structuralStatement(subj, component, TruthFunctions.implied(truth));
            } else if (compound instanceof DifferenceExt) {
                if (index == 0) {
                    structuralStatement(subj, component, TruthFunctions.implying(truth));
                } else {
                    structuralStatement(subj, component, TruthFunctions.negImply(truth));
                }
            } else if ((compound instanceof DifferenceInt) && (index == 0)) {
                structuralStatement(subj, component, TruthFunctions.implied(truth));
            }
        }
    }

    /**
     * Common final operations of the above two methods
     * @param subject The subject of the new task
     * @param predicate The predicate of the new task
     * @param truth The truth value of the new task
     */
    private static void structuralStatement(Term subject, Term predicate, TruthValue truth) {
        Task task = Memory.currentTask;
        Term content = RDFStatement.make((RDFStatement) task.getContent(), subject, predicate);
        if (content == null) {
            return;
        }
        BudgetValue budget = BudgetFunctions.compoundForward(truth, content);
        Memory.singlePremiseTask(budget, content, truth);
    }

    /* -------------------- set transform -------------------- */
    /**
     * {<S --> {P}>} |- <S <-> {P}>
     * @param compound The set compound
     * @param statement The premise
     * @param side The location of the indicated term in the premise
     */
    static void transformSetRelation(CompoundTerm compound, RDFStatement statement, short side) {
        if (compound.size() > 1) {
            return;
        }
        if (statement instanceof Inheritance) {
            if (((compound instanceof SetExt) && (side == 0)) || ((compound instanceof SetInt) && (side == 1))) {
                return;
            }
        }
        Term sub = statement.getSubject();
        Term pre = statement.getPredicate();
        Term content;
        if (statement instanceof Inheritance) {
            content = Similarity.make(sub, pre);
        } else {
            if (((compound instanceof SetExt) && (side == 0)) || ((compound instanceof SetInt) && (side == 1))) {
                content = Inheritance.make(pre, sub);
            } else {
                content = Inheritance.make(sub, pre);
            }
        }
        Task task = Memory.currentTask;
        Sentence sentence = task.getSentence();
        TruthValue truth = sentence.getTruth();
        BudgetValue budget;
        if (sentence instanceof Question) {
            budget = BudgetFunctions.compoundBackward(content);
        } else {
            budget = BudgetFunctions.compoundForward(truth, content);
        }
        Memory.singlePremiseTask(budget, content, truth);
    }

    /* -------------------- products and images transform -------------------- */
    /**
     * Equivalent transformation between products and images
     * {<(*, S, M) --> P>, S@(*, S, M)} |- <S --> (/, P, _, M)>
     * {<S --> (/, P, _, M)>, P@(/, P, _, M)} |- <(*, S, M) --> P>
     * {<S --> (/, P, _, M)>, M@(/, P, _, M)} |- <M --> (/, P, S, _)>
     * @param inh An Inheritance statement
     * @param oldContent The whole content
     * @param indices The indices of the TaskLink
     * @param task The task
     */
    static void transformProductImage(Inheritance inh, CompoundTerm oldContent, short[] indices, Task task) {
        Term subject = null;
        Term predicate = null;
        short index = indices[indices.length - 1];
        short side = indices[indices.length - 2];
        CompoundTerm comp = (CompoundTerm) inh.componentAt(side);
        if (comp instanceof Product) {
            if (side == 0) {
                subject = comp.componentAt(index);
                predicate = ImageExt.make((Product) comp, inh.getPredicate(), index);
            } else {
                subject = ImageInt.make((Product) comp, inh.getSubject(), index);
                predicate = comp.componentAt(index);
            }
        } else if ((comp instanceof ImageExt) && (side == 1)) {
            if (index == ((ImageExt) comp).getRelationIndex()) {
                subject = Product.make(comp, inh.getSubject(), index);
                predicate = comp.componentAt(index);
            } else {
                subject = comp.componentAt(index);
                predicate = ImageExt.make((ImageExt) comp, inh.getSubject(), index);
            }
        } else if ((comp instanceof ImageInt) && (side == 0)) {
            if (index == ((ImageInt) comp).getRelationIndex()) {
                subject = comp.componentAt(index);
                predicate = Product.make(comp, inh.getPredicate(), index);
            } else {
                subject = ImageInt.make((ImageInt) comp, inh.getPredicate(), index);
                predicate = comp.componentAt(index);
            }
        } else {
            return;
        }
        Inheritance newInh = Inheritance.make(subject, predicate);
        Term content = null;
        if (indices.length == 2) {
            content = newInh;
        } else if ((oldContent instanceof RDFStatement) && (indices[0] == 1)) {
            content = RDFStatement.make((RDFStatement) oldContent, oldContent.componentAt(0), newInh);
        } else {
            ArrayList<Term> componentList;
            Term condition = oldContent.componentAt(0);
            if ((oldContent instanceof Implication) && (condition instanceof Conjunction)) {
                componentList = ((CompoundTerm) condition).cloneComponents();
                componentList.set(indices[1], newInh);
                Term newCond = CompoundTerm.make((CompoundTerm) condition, componentList);
                content = Implication.make(newCond, ((RDFStatement) oldContent).getPredicate(), null);
            } else {
                componentList = oldContent.cloneComponents();
                componentList.set(indices[0], newInh);
                if (oldContent instanceof Conjunction) {
                    content = CompoundTerm.make(oldContent, componentList);
                } else if ((oldContent instanceof Implication) || (oldContent instanceof Equivalence)) {
                    content = RDFStatement.make((RDFStatement) oldContent, componentList.get(0), componentList.get(1));
                }
            }
        }
        if (content == null) {
            return;
        }
        Sentence sentence = task.getSentence();
        TruthValue truth = sentence.getTruth();
        BudgetValue budget;
        if (sentence instanceof Question) {
            budget = BudgetFunctions.compoundBackward(content);
        } else {
            budget = BudgetFunctions.compoundForward(truth, content);
        }
        Memory.singlePremiseTask(budget, content, truth);
    }

    /* --------------- Disjunction and Conjunction transform --------------- */
    /**
     * {(&&, A, B), A@(&&, A, B)} |- A
     * {(||, A, B), A@(||, A, B)} |- A
     * @param compound The premise
     * @param component The recognized component in the premise
     * @param compoundTask Whether the compound comes from the task
     */
    static void structuralCompound(CompoundTerm compound, Term component, boolean compoundTask) {
        if (!component.isConstant()) {
            return;
        }
        Term content = (compoundTask ? component : compound);
        Task task = Memory.currentTask;
        if (task.isStructual()) {
            return;
        }
        Sentence sentence = task.getSentence();
        TruthValue truth = sentence.getTruth();
        BudgetValue budget;
        if (sentence instanceof Question) {
            budget = BudgetFunctions.compoundBackward(content);
        } else {
            if ((sentence.isJudgment()) == (compoundTask == (compound instanceof Conjunction))) {
                truth = TruthFunctions.implying(truth);
            } else {
                truth = TruthFunctions.implied(truth);
            }
            budget = BudgetFunctions.forward(truth);
        }
        Memory.singlePremiseTask(budget, content, truth);
    }

    /* --------------- Negation related rules --------------- */
    /**
     * {A, A@(--, A)} |- (--, A)
     * @param content The premise
     */
    static void transformNegation(Term content) {
        Task task = Memory.currentTask;
        Sentence sentence = task.getSentence();
        TruthValue truth = sentence.getTruth();
        if (sentence instanceof Judgment) {
            truth = TruthFunctions.negation(truth);
        }
        BudgetValue budget;
        if (sentence instanceof Question) {
            budget = BudgetFunctions.compoundBackward(content);
        } else {
            budget = BudgetFunctions.compoundForward(truth, content);
        }
        Memory.singlePremiseTask(budget, content, truth);
    }

    /**
     * {<A ==> B>, A@(--, A)} |- <(--, B) ==> (--, A)>
     * @param statement The premise
     */
    static void contraposition(RDFStatement statement) {
        Term subj = statement.getSubject();
        Term pred = statement.getPredicate();
        Term content = RDFStatement.make(statement, Negation.make(pred), Negation.make(subj), TemporalValue.getReverse(statement.getOrder()));
        Task task = Memory.currentTask;
        Sentence sentence = task.getSentence();
        TruthValue truth = sentence.getTruth();
        BudgetValue budget;
        if (sentence instanceof Question) {
            if (content instanceof Implication) {
                budget = BudgetFunctions.compoundBackwardWeak(content);
            } else {
                budget = BudgetFunctions.compoundBackward(content);
            }
        } else {
            if (content instanceof Implication) {
                truth = TruthFunctions.contraposition(truth);
            }
            budget = BudgetFunctions.compoundForward(truth, content);
        }
        Memory.singlePremiseTask(budget, content, truth);
    }
}
