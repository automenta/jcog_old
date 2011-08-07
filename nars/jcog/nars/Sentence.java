/*
 * Sentence.java
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
package jcog.nars;

import jcog.nars.reason.Memory;
import jcog.nars.reason.NAR;
import jcog.nars.reason.io.Symbols;
import jcog.nars.reason.language.CompoundTerm;
import jcog.nars.reason.language.Term;


/**
 * A Sentence is an abstract class, mainly containing a Term, a TruthValue, and a Stamp.
 *<p>
 * It is used as the premises and conclusions of all inference rules.
 */
public abstract class Sentence implements Cloneable {

    /** The content of a Sentence is a Term */
    protected Term content;
    /** The punctuation also indicates the type of the Sentence: Judgment, Question, or Goal */
    protected char punctuation;
    /** The truth value of Judgment or desire value of Goal */
    protected NARTruth truth = null;
    /** Partial record of the derivation path */
    protected Stamp stamp = null;
    /** Whether it is an input sentence */
    protected boolean input = false;
    /** For Question and Goal: best solution found so far */
    protected Judgment bestSolution = null;
    /**
     * Temporal order between the components
     */
    protected TemporalValue temporalOrder = null;

    /**
     * Make a Sentence from an input String. Called by StringParser.
     * @param term The content of the sentence
     * @param punc The puncuation (and therefore, type) of the sentence
     * @param tense The tense of the sntense
     * @param truth The truth value of the sentence, if it is a Judgment (or Goal)
     * @param stamp The stamp of the truth value (for Judgment or Goal)
     * @return the Sentence generated from the arguments
     */
    public static Sentence make(Term term, char punc, NARTruth truth, Stamp stamp, TemporalValue tense) {
        if (term instanceof CompoundTerm) {
            ((CompoundTerm) term).renameVariables();
        }
        Sentence s = null;
        switch (punc) {
            case Symbols.JUDGMENT_MARK:
                s = new Judgment(term, punc, truth, stamp);
                s.temporalOrder = tense;
                break;
            case Symbols.GOAL_MARK:
                s = new Goal(term, punc, truth, stamp);
                break;
            case Symbols.QUESTION_MARK:
                s = new Question(term, punc, stamp);
                s.temporalOrder = tense;
                break;
            default:
                return null;
        }
        return s;
    }

    /**
     * Make a derived Sentence from a template and some initial values. Called by Memory.
     * @param term The content of the sentence
     * @param oldS A sample sentence providing the type of the new sentence
     * @param tense The tense of the sentence
     * @param truth The truth value of the sentence, if it is a Judgment (or Goal)
     * @param stamp The stamp of the truth value (for Judgment or Goal)
     * @return the Sentence generated from the arguments
     */
    public static Sentence make(Sentence oldS, Term term, NARTruth truth, Stamp stamp, TemporalValue tense) {
        if (term instanceof CompoundTerm) {
            ((CompoundTerm) term).renameVariables();
        }
        Sentence s = null;
        if (oldS instanceof Question) {
            s = new Question(term, Symbols.QUESTION_MARK, stamp);
            s.temporalOrder = tense;
        } else if (oldS instanceof Goal) {
            s = new Goal(term, Symbols.GOAL_MARK, truth, stamp);
        } else {
            s = new Judgment(term, Symbols.JUDGMENT_MARK, truth, stamp);
            s.temporalOrder = tense;
        }
        return s;
    }

    /**
     * Clone the Sentence
     * @return The clone
     */
    @Override
    public Object clone() {
        return make(content, punctuation, truth, stamp, temporalOrder);
    }

    /**
     * Get the content of the sentence
     * @return The content Term
     */
    public Term getContent() {
        return content;
    }

    /**
     * Clone the content of the sentence
     * @return A clone of the content Term
     */
    public Term cloneContent() {
        return (Term) content.clone();
    }

    /**
     * Set the content Term of the Sentence
     * @param t The new content
     */
    public void setContent(Term t) {
        content = t;
    }

    /**
     * Get the tense of the Sentence
     * @return The tense of the Sentence
     */
    public TemporalValue getTense() {
        return temporalOrder;
    }

    /**
     * Get the truth value of the sentence
     * @return Truth value, null for question
     */
    public NARTruth getTruth() {
        return truth;
    }

    /**
     * Get the stamp of the sentence
     * @return The stamp
     */
    public Stamp getStamp() {
        return stamp;
    }

    /**
     * Distinguish Judgment from Goal ("instanceof Judgment" doesn't work)
     * @return Whether the object is a Judgment
     */
    public boolean isJudgment() {
        return (punctuation == Symbols.JUDGMENT_MARK);
    }

    /**
     * Check input sentence
     * @return Whether the
     */
    public boolean isInput() {
        return input;
    }

    /**
     * The only place to change the default, called in StringParser
     */
    public void setInput() {
        input = true;
    }

    /**
     * Get the best-so-far solution for a Question or Goal
     * @return The stored Judgment or null
     */
    public Judgment getBestSolution() {
        return bestSolution;
    }

    /**
     * Set the best-so-far solution for a Question or Goal
     * @param judg The solution to be remembered
     */
    public void setBestSolution(Judgment judg) {
        bestSolution = judg;
        if (input) {
            Memory.report(judg, false);     // report answer to input question
        }
    }

    /**
     * Check whether one sentence has stamp overlapping with another one, and change the system cash
     * @param that The sentence to be checked against
     * @return Whether the two have overlapping stamps
     */
    public boolean noOverlapping(Memory memory, Sentence that) {
        memory.setCurrentStamp(Stamp.make(stamp, that.getStamp()));
        return (memory.getCurrentStamp() != null);
    }

    /**
     * Get a stable String representation for a Sentece
     * Different from toString: tense symbol is replaced by the actual value
     * @return String representation for a Sentece
     */
    public String toKey() {
        StringBuffer s = new StringBuffer();
        s.append(content.getName());
        s.append(punctuation + " ");
        if (temporalOrder != null) {
            s.append(temporalOrder.getDelta() + " ");
        }
        if (truth != null) {
            s.append(truth.toString());
        }
        s.append(stamp.toString());
        return s.toString();
    }

    /**
     * Get a String representation of the sentence
     * @return The String
     */
    @Override
    public String toString() {
        StringBuffer s = new StringBuffer();
        s.append(content.getName());
        s.append(punctuation + " ");
        //s.append(tenseToString());
        if (truth != null) {
            s.append(truth.toString());
        }
//        if (NARS.isStandAlone()) {
//            s.append(stamp.toString());
//            if (bestSolution != null) {
//                s.append("BestSolution: " + bestSolution);
//            }
//        }
        return s.toString();
    }

    /**
     * Get a String representation of the sentence, with 2-digit accuracy
     * @param memory 
     * @return The String
     */
    public String toString2(NAR nar) {
        StringBuffer s = new StringBuffer();
        s.append(content.getName());
        s.append(punctuation + " ");
        s.append(tenseToString(nar.getNow()));
        if (truth != null) {
            s.append(truth.toString2());
        }
//        if (NARS.isStandAlone()) {
//            s.append(stamp.toString());
//            if (bestSolution != null) {
//                s.append("BestSolution: " + bestSolution);
//            }
//        }
        return s.toString();
    }

    
    /**
     * Get a String representation of the tense of the sentence
     * @return The String
     */
    public String tenseToString(long now) {
        TemporalValue t = getTense();
        if (t == null) {
            return "";
        }
        int delta = (int) (getEventTime() - now);
        if (delta > 0) {
            return Symbols.TENSE_FUTURE + " ";
        }
        if (delta < 0) {
            return Symbols.TENSE_PAST + " ";
        }
        return Symbols.TENSE_PRESENT + " ";
    }

    /**
     * Get the creation time of the truth-value from the Stamp
     * @return The creation time of the truth-value
     */
    public long getCreationTime() {
        return getStamp().getCreationTime();
    }

    /**
     * Get the occurrence time of the event
     * @return The occurrence time of the event
     */
    public long getEventTime() {
        return getCreationTime() + getTense().getDelta();
    }
}
