/*
 * Question.java
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

import org.opencog.reason.nars.io.Symbols;
import org.opencog.reason.nars.language.Term;



/**
 * A Question is a sentence without a truth value, and may conain query variables
 */
public class Question extends Sentence {

    /**
     * COnstructor
     * @param term The content
     * @param punc The punctuation
     * @param s The stamp
     */
    public Question(Term term, char punc, Stamp s) {
        content = term;
        punctuation = punc;
        stamp = s;
    }
    
    /**
     * Construct a Judgment to indicate an operation just executed
     * @param g The goal that trigger the execution
     */
    public Question(Goal g) {
        content = g.cloneContent();
        punctuation = Symbols.QUESTION_MARK;
        temporalOrder = new TemporalValue(0);
        stamp = new Stamp();
    }
}
