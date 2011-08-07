/*
 * MatchingRules.java
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

import jcog.nars.BudgetValue;
import jcog.nars.Judgment;
import jcog.nars.NARTruth;
import jcog.nars.Question;
import jcog.nars.Sentence;
import jcog.nars.Task;
import jcog.nars.TemporalValue;
import jcog.nars.reason.Memory;
import jcog.nars.reason.language.Equivalence;
import jcog.nars.reason.language.Inheritance;
import jcog.nars.reason.language.Similarity;
import jcog.nars.reason.language.Term;



/**
 * Directly process a task by a oldBelief, with only two Terms in both. 
 * In matching, the new task is compared with all existing direct Tasks in that Concept, to carry out:
 * <p>
 * temporalRevision: between judgments on non-overlapping evidence; 
 * temporalRevision: between judgments; 
 * satisfy: between a Judgment and a Question/Goal; 
 * merge: between items of the same type and stamp;
 * conversion: between different inheritance relations.
 */
public final class MatchingRules {

    /* -------------------- same contents -------------------- */
    /**
     * The task and belief have the same content
     * <p>
     * called in RuleTables.reason
     * @param task The task
     * @param belief The belief
     */
    public static void match(Task task, Judgment belief) {
        Sentence sentence = task.getSentence();
        if (sentence.isJudgment()) {
            revision(task, belief, true);
        } else {
            trySolution(sentence, belief, null);
        }
    }

    /**
     * Belief revision
     * <p>
     * called from Concept.reviseTable and match
     * @param task The task containing new belief
     * @param oldBelief The previous belief with the same content
     * @param feedbackToLinks Whether to send feedback to the links
     * @return Whether temporalRevision happened
     */
    public static boolean revision(Task task, Judgment oldBelief, boolean feedbackToLinks) {
        Judgment newBelief = (Judgment) task.getSentence();
        if (TemporalRules.sameTime(newBelief, oldBelief)) {
            NARTruth tTruth = newBelief.getTruth();
            NARTruth bTruth = oldBelief.getTruth();
            NARTruth truth = TruthFunctions.revision(tTruth, bTruth);
            BudgetValue budget = BudgetFunctions.revise(tTruth, bTruth, truth, task, feedbackToLinks);
            Term content = newBelief.getContent();
            Memory.doublePremiseTask(budget, content, truth);
            return true;
        } else {
            return false;
        }
    }

    /**
     * Check if a Judgment provide a better answer to a Question
     * @param problem The Goal or Question to be answered
     * @param belief The proposed answer
     * @param task The task to be processed
     */
    public static void trySolution(Memory m, Sentence problem, Judgment belief, Task task) {
        Judgment oldBest = problem.getBestSolution();
        if (betterSolution(belief, oldBest, problem)) {
            problem.setBestSolution(belief);
            BudgetValue budget = BudgetFunctions.solutionEval(m, problem, belief, task);
            if ((budget != null) && budget.aboveThreshold(m)) {
                m.activatedTask(budget, belief, problem.isInput());
            }
        }
    }

    /**
     * Compare the quality of two solutions
     * @param newSol A new solution
     * @param oldSol A old solution
     * @param problem The problem
     * @return Whether the new one is better
     */
    private static boolean betterSolution(Judgment newSol, Judgment oldSol, Sentence problem) {
        if (oldSol == null) {
            return true;
        } else {
            if ((problem instanceof Question) && (problem.getTense() != null)) {
                if(TemporalValue.closer(newSol.getTense(), oldSol.getTense(), problem.getTense())) {
                    return true;
                }
            }
            return (newSol.solutionQuality(problem) > oldSol.solutionQuality(problem));
        }
    }

    /* -------------------- same terms, difference relations -------------------- */
    /**
     * The task and belief match reversely
     */
    public static void matchReverse() {
        Task task = Memory.currentTask;
        Judgment belief = Memory.currentBelief;
        TemporalValue t1 = task.getContent().getOrder();
        TemporalValue t2 = belief.getContent().getOrder();
        if (((t1 == null) && (t2 != null)) || ((t1 != null) && (t2 == null))) {
            return;
        }
        if ((t1 != t2) && (t1.getDelta() + t2.getDelta() != 0)) {
            return;
        }
        Sentence sentence = task.getSentence();
        if (sentence.isJudgment()) {
            inferToSym((Judgment) sentence, belief);
        } else {
            conversion();
        }
    }

    /**
     * Inheritance/Implication matches Similarity/Equivalence
     * @param asym A Inheritance/Implication sentence
     * @param sym A Similarity/Equivalence sentence
     * @param figure location of the shared term
     */
    public static void matchAsymSym(Sentence asym, Sentence sym, int figure) {
        TemporalValue order1 = asym.getContent().getOrder();
        TemporalValue order2 = sym.getContent().getOrder();
        TemporalValue order = TemporalRules.syllogistic(order1, order2, figure);
        if ((order1 != order2) && (order == null)) {
            return;
        }
        if (Memory.currentTask.getSentence().isJudgment()) {
            inferToAsym((Judgment) asym, (Judgment) sym, order);
        } else {
            convertRelation();
        }
    }

    /* -------------------- two-premise inference rules -------------------- */
    /**
     * {<S --> P>, <P --> S} |- <S <-> p>
     * Produce Similarity/Equivalence from a pair of reversed Inheritance/Implication
     * @param judgment1 The first premise
     * @param judgment2 The second premise
     */
    private static void inferToSym(Judgment judgment1, Judgment judgment2) {
        RDFStatement s1 = (RDFStatement) judgment1.getContent();
        Term t1 = s1.getSubject();
        Term t2 = s1.getPredicate();
        Term content;
        if (s1 instanceof Inheritance) {
            content = Similarity.make(t1, t2);
        } else {
            TemporalValue order = s1.getOrder();
            if ((order != null) && order.getDelta() < 0)
                content = Equivalence.make(t1, t2, TemporalValue.getReverse(order));
            else
                content = Equivalence.make(t1, t2, order);
        }
        NARTruth value1 = judgment1.getTruth();
        NARTruth value2 = judgment2.getTruth();
        NARTruth truth = TruthFunctions.intersection(value1, value2);
        BudgetValue budget = BudgetFunctions.forward(truth);
        Memory.doublePremiseTask(budget, content, truth);
    }

    /**
     * {<S <-> P>, <P --> S>} |- <S --> P>
     * Produce an Inheritance/Implication from a Similarity/Equivalence and a reversed Inheritance/Implication
     * @param asym The asymmetric premise
     * @param sym The symmetric premise
     */
    private static void inferToAsym(Judgment asym, Judgment sym, TemporalValue order) {
        RDFStatement statement = (RDFStatement) asym.getContent();
        Term sub = statement.getPredicate();
        Term pre = statement.getSubject();
        RDFStatement content = RDFStatement.make(statement, sub, pre, order);
        NARTruth truth = TruthFunctions.reduceConjunction(sym.getTruth(), asym.getTruth());
        BudgetValue budget = BudgetFunctions.forward(truth);
        Memory.doublePremiseTask(budget, content, truth);
    }

    /* -------------------- one-premise inference rules -------------------- */
    /**
     * {<P --> S>} |- <S --> P>
     * Produce an Inheritance/Implication from a reversed Inheritance/Implication
     */
    private static void conversion() {
        NARTruth truth = TruthFunctions.conversion(Memory.currentBelief.getTruth());
        BudgetValue budget = BudgetFunctions.forward(truth);
        Memory.convertedJudgment(truth, budget);
    }

    /**
     * {<S --> P>} |- <S <-> P>
     * {<S <-> P>} |- <S --> P>
     * Switch between Inheritance/Implication and Similarity/Equivalence
     */
    private static void convertRelation() {
        NARTruth truth = Memory.currentBelief.getTruth();
        if (((RDFStatement) Memory.currentTask.getContent()).isCommutative()) {
            truth = TruthFunctions.implied(truth);
        } else {
            truth = TruthFunctions.implying(truth);
        }
        BudgetValue budget = BudgetFunctions.forward(truth);
        Memory.convertedJudgment(truth, budget);
    }
}
