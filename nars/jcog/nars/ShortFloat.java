/*
 * ShortFloat.java
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

/**
 * A float value in [0, 1], with 4 digits accuracy.
 */
public class ShortFloat implements Cloneable {

    /** To save space, the values are stored as short integers (-32768 to 32767, only 0 to 10000 used),
    but used as float */
    private short value;

    /** 
     * Constructor
     * @param v The initial value
     */
    public ShortFloat(float v) {
        setValue(v);
    }

    /**
     * To access the value as float
     * @return The current value in float
     */
    public float getValue() {
        return (float) (value * 0.0001);
    }

    /**
     * To access the value as short
     * @return The current value in short
     */
    short getShortValue() {
        return value;
    }

    /**
     * Set new value, rounded, with validity checking
     * @param v The new value
     */
    public void setValue(float v) {
        if ((v < 0) || (v > 1)) {
            System.out.println("!!! Wrong value: " + v);
        } else {
            value = (short) (v * 10000.0 + 0.5);
        }
    }

    /**
     * Compare two ShortFloat values
     * @param that The other value to be compared
     * @return Whether the two have the same value
     */
    @Override
    public boolean equals(Object that) {
        return ((that instanceof ShortFloat) && (value == ((ShortFloat) that).getShortValue()));
    }

    /**
     * The hash code of the ShortFloat
     * @return The hash code
     */
    @Override
    public int hashCode() {
        int hash = 3;
        hash = 17 * hash + this.value;
        return hash;
    }

    /**
     * Convert the value into a String
     * @return The String representation, with 4 digits accuracy
     */
    @Override
    public String toString() {
        if (value >= 10000) {
            return "1.0000";
        } else {
            String s = String.valueOf(value);
            while (s.length() < 4) {
                s = "0" + s;
            }
            return "0." + s;
        }
    }

    /**
     * Round the value into a short String
     * @return The String representation, with 2 digits accuracy
     */
    public String toString2() {
        value += 50;
        String s = toString();
        value -= 50;
        if (s.length() > 4) {
            return s.substring(0, 4);
        } else {
            return s;
        }
    }
}
