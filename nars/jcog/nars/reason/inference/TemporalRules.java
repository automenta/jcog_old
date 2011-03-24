/*
 * TemporalRules.java
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

import org.opencog.atom.nars.BudgetValue;
import org.opencog.atom.nars.Judgment;
import org.opencog.atom.nars.TemporalValue;
import org.opencog.atom.nars.TruthValue;
import org.opencog.reason.nars.Memory;
import org.opencog.reason.nars.language.Term;
/**
 * Temporal inference rules
 * <p> 
 * These rules are independent of the semantic and symtactic inference rules.
 */
public class TemporalRules {

    /**
     * Temporal relationships among three terms
     * @param r12 Temporal TemporalValueship from term1 to term2
     * @param r23 Temporal TemporalValueship from term2 to term3
     * @return Temporal TemporalValueship from term1 to term3
     */
    public static TemporalValue syllogistic(TemporalValue r12, TemporalValue r23) {
        if ((r12 == null) && (r23 == null)) {
            return null;
        }
        int i12 = (r12 == null) ? 0 : r12.getDelta();
        int i23 = (r23 == null) ? 0 : r23.getDelta();
        int i13 = i12 + i23;
        return new TemporalValue(i13);
    }

    /**
     * Temporal relationships among three terms, with creasion time
     * @param r12 Temporal TemporalValueship from term1 to term2
     * @param time1 Creation time of the first premise
     * @param r23 Temporal TemporalValueship from term2 to term3
     * @return Temporal TemporalValueship from term1 to term3
     */
    public static TemporalValue tenseSyllogistic(TemporalValue r12, long time1, TemporalValue r23) {
        if ((r12 == null) && (r23 == null)) {
            return null;
        }
        int i12 = (r12 == null) ? 0 : r12.getDelta();
        int i23 = (r23 == null) ? 0 : r23.getDelta();
        int i13 = (int) (time1 + i12 + i23 - Center.getTime());
        return new TemporalValue(i13);
    }

    /**
     * Temporal inference in syllogism, with figure
     * @param r1 The first premise, containing the subject of the conclusion
     * @param r2 The second premise, containing the predicate of the conclusion
     * @param figure The location of the shared term
     * @return The temporal order in the conclusion
     */
    public static TemporalValue syllogistic(TemporalValue r1, TemporalValue r2, int figure) {
        if ((r1 == null) && (r2 == null)) {
            return null;
        }
        switch (figure) {
            case 11:
                return syllogistic(TemporalValue.getReverse(r1), r2);
            case 12:
                return syllogistic(TemporalValue.getReverse(r1), TemporalValue.getReverse(r2));
            case 21:
                return syllogistic(r1, r2);
            case 22:
                return syllogistic(r1, TemporalValue.getReverse(r2));
            default:
                return null;
        }
    }

    // called in variable introduction
    /**
     * The temporal inference on tenses
     * @param tense1 The tense of the first premise
     * @param tense2 The tense of the second premise
     * @return The tense of the conclusion
     */
    public static TemporalValue tenseInduction(TemporalValue tense1, TemporalValue tense2) {
        if ((tense1 == null) || (tense2 == null)) {
            return null;
        }
        int i1 = tense1.getDelta();
        int i2 = tense2.getDelta();
        int i = i2 - i1;
        return new TemporalValue(i);
    }
    
    /**
     * Belief temporalRevision
     * <p>
     * called from Concept.reviseTable and match
     * @param newBelief The new belief
     * @param oldBelief The previous belief with the same content
     * @param time The target time of the statement
     * @param feedbackToLinks Whether to send feedback to the links
     * @return Whether temporalRevision happened
     */
    public static boolean temporalRevision(Judgment newBelief, Judgment oldBelief, long time, boolean feedbackToLinks) {
        TruthValue tTruth = newBelief.getTruth();
        TruthValue bTruth = oldBelief.getTruth();
        long time1 = newBelief.getCreationTime();
        long time2 = oldBelief.getCreationTime();
        TruthValue truth = TruthFunctions.temporalRevision(tTruth, bTruth, time1, time2, time);            
        BudgetValue budget = BudgetFunctions.revise(tTruth, bTruth, truth, Memory.currentTask, feedbackToLinks);
        Term content = newBelief.getContent();
        Memory.currentTense = new TemporalValue((int) (time - Center.getTime()));
        Memory.doublePremiseTask(budget, content, truth);
        return true;
    }
    
    /**
     * If the two beliefs have compatible temporal information
     * @param belief1
     * @param belief2
     * @return If the two can be considered as about the same time
     */
    public static boolean sameTime(Judgment belief1, Judgment belief2) {
        TemporalValue tense1 = belief1.getTense();
        TemporalValue tense2 = belief2.getTense();
        if ((tense1 == null) || (tense2 == null)) {
            Memory.currentTense = null;
        } else {
            long time1 = belief1.getEventTime();
            long time2 = belief2.getEventTime();
            if (time1 != time2) {
                return false;
            }
            long time = Center.getTime();
            Memory.currentTense = new TemporalValue((int) (time1 - time));
        }
        return true;
    }
}
