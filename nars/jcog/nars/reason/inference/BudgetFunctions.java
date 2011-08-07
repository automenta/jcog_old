/*
 * BudgetFunctions.java
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
import jcog.nars.Concept;
import jcog.nars.Goal;
import jcog.nars.Judgment;
import jcog.nars.NARTruth;
import jcog.nars.Question;
import jcog.nars.Sentence;
import jcog.nars.Task;
import jcog.nars.TaskLink;
import jcog.nars.TermLink;
import jcog.nars.TruthValue;
import jcog.nars.reason.Memory;
import jcog.nars.reason.language.Term;



/**
 * Budget functions for resources allocation
 */
public final class BudgetFunctions extends UtilityFunctions {

    /* ----------------------- Belief evaluation ----------------------- */
    /**
     * Determine the quality of a judgment by its truth value alone
     * <p>
     * Mainly decided by confidence, though binary judgment is also preferred
     * @param t The truth value of a judgment
     * @return The quality of the judgment, according to truth value only
     */
    public static float truthToQuality(NARTruth t) {
        float freq = t.getFrequency();
        float conf = t.getConfidence();
        return aveGeo(conf, Math.abs(freq - 0.5f) + freq * 0.5f);
    }

    /**
     * Determine the rank of a judgment by its confidence and originality (stamp length)
     * @param judg The judgment to be ranked
     * @return The rank of the judgment, according to truth value only
     */
    public static float rankBelief(Judgment judg) {
        float confidence = judg.getTruth().getConfidence();
        float originality = 1.0f / (judg.getStamp().length() + 1);
        return or(confidence, originality);
    }

    /* ----- Functions used both in direct and indirect processing of tasks ----- */
    /**
     * Evaluate the quality of a belief as a solution to a problem, then reward 
     * the belief and de-prioritize the problem
     * @param problem The problem (question or goal) to be solved
     * @param solution The belief as solution
     * @param task The task to be immediatedly processed, or null for continued process
     * @return The budget for the new task which is the belief activated, if necessary
     */
    static BudgetValue solutionEval(Memory memory, Sentence problem, Judgment solution, Task task) {
        BudgetValue budget = null;
        boolean feedbackToLinks = false;
        if (task == null) {                   // called in continued processing
            task = memory.getCurrentTask();
            feedbackToLinks = true;
        }
        boolean judgmentTask = task.getSentence().isJudgment();
        float quality;
        if (problem instanceof Question) {
            quality = solution.solutionQuality((Question) problem);
        } else {
            assert (problem instanceof Goal);
            quality = solution.getTruth().getExpectation();
        }
        if (judgmentTask) {
            task.incPriority(quality);
        } else {
            task.setPriority(Math.min(1 - quality, task.getPriority()));
            budget = new BudgetValue(quality, task.getDurability(), truthToQuality(solution.getTruth()));
        }
        if (feedbackToLinks) {
            TaskLink tLink = memory.currentTaskLink;
            tLink.setPriority(Math.min(1 - quality, tLink.getPriority()));
            TermLink bLink = memory.currentBeliefLink;
            bLink.incPriority(quality);
        }
        return budget;
    }

    /**
     * Evaluate the quality of a revision, then de-prioritize the premises
     * @param tTruth The truth value of the judgment in the task
     * @param bTruth The truth value of the belief
     * @param truth The truth value of the conclusion of revision
     * @param task The task to be immediatedly or continuely processed
     * @return The budget for the new task 
     */
    static BudgetValue revise(Memory memory, TruthValue tTruth, TruthValue bTruth, TruthValue truth, Task task, boolean feedbackToLinks) {
        float difT = truth.getExpDifAbs(tTruth);
        task.decPriority(1 - difT);
        task.decDurability(1 - difT);
        if (feedbackToLinks) {
            TaskLink tLink = memory.currentTaskLink;
            tLink.decPriority(1 - difT);
            tLink.decDurability(1 - difT);
            TermLink bLink = memory.currentBeliefLink;
            float difB = truth.getExpDifAbs(bTruth);
            bLink.decPriority(1 - difB);
            bLink.decDurability(1 - difB);
        }
        float dif = truth.getConfidence() - Math.max(tTruth.getConfidence(), bTruth.getConfidence());
        float priority = or(dif, task.getPriority());
        float durability = or(dif, task.getDurability());
        float quality = truthToQuality(truth);
        return new BudgetValue(priority, durability, quality);
    }

    /**
     * Update a belief
     * @param task The task containing new belief
     * @param bTruth Truth value of the previous belief
     * @return Budget value of the updating task
     */
    static BudgetValue update(Task task, NARTruth bTruth) {
        NARTruth tTruth = task.getSentence().getTruth();
        float dif = tTruth.getExpDifAbs(bTruth);
        float priority = or(dif, task.getPriority());
        float durability = or(dif, task.getDurability());
        float quality = truthToQuality(bTruth);
        return new BudgetValue(priority, durability, quality);
    }

    /* ----------------------- Links ----------------------- */
    /**
     * Distribute the budget of a task among the links to it
     * @param b The original budget
     * @param n Number of links
     * @return Budget value for each link
     */
    public static BudgetValue distributeAmongLinks(BudgetValue b, int n) {
        float priority = (float) (b.getPriority() / Math.sqrt(n));
        return new BudgetValue(priority, b.getDurability(), b.getQuality());
    }

    /* ----------------------- Concept ----------------------- */
    /**
     * Activate a concept by an incoming TaskLink
     * @param concept The concept
     * @param budget The budget for the new item 
     */
    public static void activate(Concept concept, BudgetValue budget) {
        float oldPri = concept.getPriority();
        float priority = or(oldPri, budget.getPriority());
        float durability = aveAri(concept.getDurability(), budget.getDurability(), oldPri / priority);
        float quality = concept.getQuality();
        concept.setPriority(priority);
        concept.setDurability(durability);
        concept.setQuality(quality);
    }

    /* ---------------- Bag functions, on all Items ------------------- */
    /**
     * Decrease Priority after an item is used, called in Bag
     * <p>
     * After a constant time, p should become d*p.  Since in this period, the item is accessed c*p times, 
     * each time p-q should multiple d^(1/(c*p)). 
     * The intuitive meaning of the parameter "forgetRate" is: after this number of times of access, 
     * priority 1 will become d, it is a system parameter adjustable in run time.
     *
     * @param budget The previous budget value
     * @param forgetRate The budget for the new item
     * @param relativeThreshold The relative threshold of the bag
     */
    public static void forget(BudgetValue budget, float forgetRate, float relativeThreshold) {
        double quality = budget.getQuality() * relativeThreshold;      // re-scaled quality
        double p = budget.getPriority() - quality;                     // priority above quality
        if (p > 0) {
            quality += p * Math.pow(budget.getDurability(), 1.0 / (forgetRate * p));
        }    // priority Durability
        budget.setPriority((float) quality);
    }

    /**
     * Merge an item into another one in a bag, when the two are identical except in budget values
     * @param baseValue The budget value to be modified
     * @param adjustValue The budget doing the adjusting
     */
    public static void merge(BudgetValue baseValue, BudgetValue adjustValue) {
        baseValue.incPriority(adjustValue.getPriority());
        baseValue.setDurability(Math.max(baseValue.getDurability(), adjustValue.getDurability()));
        baseValue.setQuality(Math.max(baseValue.getQuality(), adjustValue.getQuality()));
    }

    /* ----- Task derivation in MatchingRules and SyllogisticRules ----- */
    /**
     * Forward inference result and adjustment
     * @param truth The truth value of the conclusion
     * @return The budget value of the conclusion
     */
    static BudgetValue forward(Memory m, NARTruth truth) {
        return budgetInference(m, truthToQuality(truth), 1);
    }

    /**
     * Backward inference result and adjustment, stronger case
     * @param truth The truth value of the belief deriving the conclusion
     * @return The budget value of the conclusion
     */
    public static BudgetValue backward(Memory m, NARTruth truth) {
        return budgetInference(m, truthToQuality(truth), 1);
    }

    /**
     * Backward inference result and adjustment, weaker case
     * @param truth The truth value of the belief deriving the conclusion
     * @return The budget value of the conclusion
     */
    public static BudgetValue backwardWeak(Memory m, NARTruth truth) {
        return budgetInference(m, w2c(1) * truthToQuality(truth), 1);
    }

    /* ----- Task derivation in CompositionalRules and StructuralRules ----- */
    /**
     * Forward inference with CompoundTerm conclusion
     * @param truth The truth value of the conclusion
     * @param content The content of the conclusion
     * @return The budget of the conclusion
     */
    public static BudgetValue compoundForward(Memory memory, NARTruth truth, Term content) {
        return budgetInference(memory, truthToQuality(truth), content.getComplexity());
    }

    /**
     * Backward inference with CompoundTerm conclusion, stronger case
     * @param content The content of the conclusion
     * @return The budget of the conclusion
     */
    public static BudgetValue compoundBackward(Memory memory, Term content) {
        return budgetInference(memory, 1, content.getComplexity());
    }

    /**
     * Backward inference with CompoundTerm conclusion, weaker case
     * @param content The content of the conclusion
     * @return The budget of the conclusion
     */
    public static BudgetValue compoundBackwardWeak(Memory memory, Term content) {
        return budgetInference(memory, w2c(1), content.getComplexity());
    }

    /**
     * Common processing for all inference step
     * @param qual Quality of the inference
     * @param complexity Syntactic complexity of the conclusion
     * @return Budget of the conclusion task
     */
    private static BudgetValue budgetInference(Memory memory, float qual, int complexity) {
        TaskLink tLink = memory.currentTaskLink;
        TermLink bLink = memory.currentBeliefLink;
        float priority = tLink.getPriority();
        float durability = tLink.getDurability();
        float quality = (float) (qual / Math.sqrt(complexity));
        if (bLink != null) {
            priority = aveAri(priority, bLink.getPriority());
            durability = aveAri(durability, bLink.getDurability());
            bLink.incPriority(quality);
        }
        return new BudgetValue(and(priority, quality), and(durability, quality), quality);
    }

    /**
     * Special treatment for temporal induction and comparison among recent event, no feedback
     * @param b1 Budget value of the first premise
     * @param b2 Budget value of the second premise
     * @param t Truth value of the conclusion
     * @return Budget value of the conclusion task
     */
    public static BudgetValue temporalIndCom(BudgetValue b1, BudgetValue b2, TruthValue t) {
        float priority = and(b1.getPriority(), b2.getPriority());
        float durability = and(b1.getDurability(), b2.getDurability());
        float quality = t.getConfidence();
        return new BudgetValue(priority, durability, quality);
    }
}
