/*
 * Goal.java
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

import org.opencog.reason.nars.language.Term;




/**
 * A Goal is an event to be realized, and may conain query variables
 */
public class Goal extends Judgment {
    /**
     * Constructor
     * <p>
     * A goal has no tense
     * @param term The content
     * @param punc The punctuation
     * @param t The desire (truth) value
     * @param s The stamp
     */
    public Goal(Term term, char punc, NARTruth t, Stamp s) {
        super(term, punc, t, s);
    }
}

