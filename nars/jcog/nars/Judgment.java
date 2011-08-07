/*
 * Judgment.java
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

import jcog.nars.reason.NARParams;
import jcog.nars.reason.io.Symbols;
import jcog.nars.reason.language.Term;




/**
 * A Judgment is an piece of new knowledge to be absorbed.
 */
public class Judgment extends Sentence {

    /**
     * Constructor
     * @param term The content
     * @param punc The punctuation
     * @param t The truth value
     * @param b The stamp
     */
    public Judgment(Term term, char punc, NARTruth t, Stamp b) {
    	super();
        content = term;
        punctuation = punc;
        truth = t;
        stamp = b;
    }

    /**
     * Construct a Judgment to indicate an operation just executed
     * @param g The goal that trigger the execution
     */
    public Judgment(NARParams params, Goal g) {
        content = g.cloneContent();
        punctuation = Symbols.JUDGMENT_MARK;
        temporalOrder = new TemporalValue(0);
        truth = new NARTruth(1.0f, params.getDefaultJudgmentConfidence());
        stamp = new Stamp();
    }

    /**
     * Check whether the judgment is equivalent to another one
     * <p>
     * The two may have different keys
     * @param that The other judgment
     * @return Whether the two are equivalent
     */
    boolean equivalentTo(Judgment that) {
        assert content.equals(that.getContent());
        return (truth.equals(that.getTruth()) && stamp.equals(that.getStamp()) 
                && TemporalValue.equal(temporalOrder, that.getTense())); 
    }

    /**
     * Evaluate the quality of the judgment as a solution to a problem
     * @param problem A goal or question
     * @return The quality of the judgment as the solution
     */
    public float solutionQuality(Sentence problem) {
        if (problem instanceof Goal) {
            return truth.getExpectation();
        } else if (problem.getContent().isConstant()) {   // "yes/no" question
            return truth.getConfidence();
        } else {                                    // "what" question or goal
            return truth.getExpectation() / content.getComplexity();
        }
    }
    
    /**
     * Check if the judgment predict a future event
     * @param now 
     * @return Whether the judgment has a future sense
     */
    public boolean isFuture(long now) {
        if (temporalOrder == null)
            return false;
        long eventTime = stamp.getCreationTime() + temporalOrder.getDelta();
        return eventTime > now/*nars.main.Center.getTime()*/;
    }
    
    
}

