/*
 * TemporalValue.java
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

import jcog.nars.reason.io.Symbols;



/**
 * Temporal information, in terms of the internal clock of the system. 
 * In an Implication or Equivalence, the value indicates the interval from the first event to the second. 
 * In a Conjunction, the value can only be 0 (parallel) or 1 (suquential). 
 * In a Sentence, the value indicates the interval from when the sentence is created to when the event happens.
 * @author Pei Wang
 */
public class TemporalValue {
    private int delta;
    
    /**
     * Constructor, with a given number
     * @param t The temporal difference of the item
     */
    public TemporalValue(int t) {
        delta = t;
    }
    
    /**
     * Constructor, with a String representation
     * @param s The temporal difference of the item as a String
     */
    public TemporalValue(String s) {
        if (s.equals(Symbols.TENSE_FUTURE) || s.equals(Symbols.SEQUENCE_OPERATOR)
         || s.equals(Symbols.IMPLICATION_AFTER_RELATION) || s.equals(Symbols.EQUIVALENCE_AFTER_RELATION))
            delta = 1;
        else if (s.equals(Symbols.TENSE_PAST) || s.equals(Symbols.IMPLICATION_BEFORE_RELATION))
            delta = -1;
        else
            delta = 0;
    }
    
    /**
     * Directly report the time difference
     * @return The delta value
     */
    public int getDelta() {
        return delta;
    }
    
    /**
     * Get the reverse temporal difference of a given one
     * @param t The given temporal value
     * @return The reverse temporal value
     */
    public static TemporalValue getReverse(TemporalValue t) {
        if (t == null) {
            return null;
        } else {
            return new TemporalValue(0 - t.getDelta());
        }
    }
    
    /**
     * Check if two temporal values have the same content
     * @param t1 The first temporal value
     * @param t2 The second temporal value
     * @return If the two dicuate the same temporal relation
     */
    public static boolean equal(TemporalValue t1, TemporalValue t2) {
        if (t1 == t2) {
            return true;
        }
        if ((t1 == null) || (t2 == null)) {
            return false;
        }
        return (t1.getDelta() == t2.getDelta());
    }
    
    /**
     * Check if the first temporal valie is closer to the given time than the second
     * @param t1 The first temporal value
     * @param t2 The second temporal value
     * @param t The target temporal value
     * @return If the first is closer to the target
     */
    public static boolean closer(TemporalValue t1, TemporalValue t2, TemporalValue t) {
        if (t1 != null) {
            if (t2 == null) {
                return true;
            } else {
                int time1 = t1.getDelta();
                int time2 = t2.getDelta();
                int time = t.getDelta();
                return (Math.abs(time1 - time) < Math.abs(time2 - time));
            }
        }
        return false;
    }
}
