/*
 * Stamp.java
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

import java.util.*;
import jcog.nars.reason.NARParams;
import jcog.nars.reason.io.Symbols;




/**
 * Each Sentence has a get of serial numbers of a constant length attached.
 * The input sentence gets a unique number.
 * The derived sentences inherits from its parents, then cut at the length limit.
 */
public class Stamp implements Cloneable {
    /** serial number, for the whole system */
    private static long current = 0;
    
    /** serial numbers */
    private long[] list;
    
    /** current stamp length */
    private int length;
    
    /** creation time of the stamp */
    private final long creationTime = new Date().getTime();

    /**
     * Generate a new stamp, with a new serial number, for input sentence
     */
    public Stamp() {
    	super();
    	
        current++;
        length = 1;
        list = new long[length];
        list[0] = current;
    }

    /**
     * Generate a new stamp for derived sentence by merging the two from parents
     * the first one is no shorter than the second
     * @param first The first Stamp
     * @param second The second Stamp
     */
    private Stamp(Stamp first, Stamp second) {
        int i1, i2, j;
        i1 = i2 = j = 0;
        length = Math.min(first.length() + second.length(), NARParams.MAXMUM_STAMP_LENGTH);
        list = new long[length];
        while (i2 < second.length() && j < length) {
            list[j] = first.get(i1);
            i1++;
            j++;
            list[j] = second.get(i2);
            i2++;
            j++;
        }
        while (i1 < first.length() && j < length) {
            list[j] = first.get(i1);
            i1++;
            j++;
        }
    }

    /**
     * Try to merge two Stamps, return null if have overlap
     * @param first The first Stamp
     * @param second The second Stamp
     * @return The merged Stamp, or null
     */
    public static Stamp make(Stamp first, Stamp second) {
        for (int i = 0; i < first.length(); i++) {
            for (int j = 0; j < second.length(); j++) {
                if (first.get(i) == second.get(j)) {
                    return null;
                }
            }
        }
        if (first.length() > second.length()) {
            return new Stamp(first, second);
        } else {
            return new Stamp(second, first);
        }
    }

    /**
     * Initialize the stamp machenism of the system, called in Center
     */
    public static void init() {
        current = 0;
    }

    /**
     * Return the length of the list
     * @return Length of the Stamp
     */
    public int length() {
        return length;
    }

    /**
     * Get a number from the list by index, called in this class only
     * @param i The index
     * @return The number at the index
     */
    long get(int i) {
        return list[i];
    }

    /**
     * Convert the list into a set
     */
    TreeSet<Long> toSet() {
        TreeSet<Long> set = new TreeSet<Long>();
        for (int i = 0; i < length; i++) {
            set.add(list[i]);
        }
        return set;
    }

    /**
     * Check if two stamps contains the same set of numbers
     * @param that The Stamp to be compared
     * @return Whether the two have contain the same elements
     */
    @Override
    public boolean equals(Object that) {
        if (!(that instanceof Stamp)) {
            return false;
        }
        TreeSet<Long> set1 = toSet();
        TreeSet<Long> set2 = ((Stamp) that).toSet();
        return (set1.containsAll(set2) && set2.containsAll(set1));
    }

    /**
     * The hash code of Stamp
     * @return The hash code
     */
    @Override
    public int hashCode() {
        int hash = 7;
        return hash;
    }

    /**
     * Get the creationTime of the truth-value
     * @return The creation time
     */
    public long getCreationTime() {
        return creationTime;
    }

    /**
     * Compare two Stamps for their temporal order
     * @param that The Stamp to be compared
     * @return The temporal order
     */
    public TemporalValue orderWith(Stamp that) {
        int delta = (int) (creationTime - that.getCreationTime());
        return new TemporalValue(delta);
    }

    /**
     * Get a String form of the Stamp for display
     * @return The Stamp as a String
     */
    @Override public String toString() {
        StringBuffer buffer = new StringBuffer(" " + Symbols.STAMP_OPENER + creationTime + Symbols.STAMP_STARTER + " ");
        for (int i = 0; i < length; i++) {
            buffer.append(Long.toString(list[i]));
            if (i < (length - 1)) {
                buffer.append(Symbols.STAMP_SEPARATOR + "");
            } else {
                buffer.append(Symbols.STAMP_CLOSER + " ");
            }
        }
        return buffer.toString();
    }
}
