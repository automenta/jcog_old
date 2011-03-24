/*
 * Item.java
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
 * An item is an object that can be put into a Bag,
 * to participate in the resource competation of the system.
 * <p>
 * It has a key and a budget.
 */
public abstract class Item extends BudgetValue {

    /** The key of the Item, unique in a Bag */
    protected String key;   // uniquely define an Item in a bag

    /**
     * Default constructor
     */
    protected Item() {
        super();
    }

    /**
     * Constructor with initial budget
     * @param v The initial budget
     */
    protected Item(BudgetValue v) {
        super(v);
    }

    /**
     * Get the current key
     * @return Current key value
     */
    public String getKey() {
        return key;
    }

    /**
     * Get current BudgetValue
     * <p>
     * This method is redundant, just to make the code more readable
     * @return Current BudgetValue
     */
    public BudgetValue getBudget() {
        return this;
    }
}
