/*
 * Bag.java
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
package jcog.nars.reason.store;

import java.util.ArrayList;
import java.util.HashMap;

import org.opencog.atom.nars.Item;
import org.opencog.reason.nars.inference.BudgetFunctions;



/**
 * A Bag is a storage with a constant capacity and maintains an internal priority
 * distribution for retrieval.
 * <p>
 * Each entity in a bag must extend Item, which has a BudgetValue and a key.
 * <p>
 * A name table is used to merge duplicate items that have the same key.
 * <p>
 * The bag space is divided by a threshold, above which is mainly time management,
 * and below, space mamagement.
 * Differences: (1) level selection vs. item selection, (2) decay rate
 * @param Type The type of the Item in the Bag
 */
public abstract class Bag<Type extends Item> {

	/** priority levels */
	protected int TOTAL_LEVEL;
	
	/** firing threshold */
	protected int THRESHOLD;

	/** relative threshold, only calculate once */
	protected float RELATIVE_THRESHOLD;
	
	/** hashtable load factor */
	protected float LOAD_FACTOR; 

	/** shared distributor that produce the probability distribution */
	Distributor DISTRIBUTOR;
	
	/** mapping from key to item */
	protected HashMap<String, Type> nameTable;
	
	/** array of lists of items, for items on different level */	
	protected ArrayList<Type> itemTable[];
	
	
	/** current sum of occupied level */
	protected int mass = 0;
	
	/** current take out level */
	protected int currentLevel;
	
	/** maximum number of items to be taken out at current level */
	protected int currentCounter;

	private int capacity;

	


	/**
	 * constructor, called from subclasses
	 */
	protected Bag(float loadFactor, int threshold, int totalLevel, int capacity) {
		super();

		this.capacity = capacity;
		
		this.LOAD_FACTOR = loadFactor; // = Parameters.LOAD_FACTOR;
		this.THRESHOLD = threshold; // = Parameters.BAG_THRESHOLD
		this.TOTAL_LEVEL = totalLevel;	// = Parameters.BAG_LEVEL
		this.RELATIVE_THRESHOLD = (float) THRESHOLD / (float) TOTAL_LEVEL;
		
		this.DISTRIBUTOR = new Distributor(TOTAL_LEVEL);
		
		currentLevel = TOTAL_LEVEL - 1;
		
		itemTable = new ArrayList[TOTAL_LEVEL];
		
		nameTable = new HashMap<String, Type>((int) (capacity / LOAD_FACTOR), LOAD_FACTOR);
	}

	/** index to get next level, kept in individual objects */
	public int getLevelIndex() {
		return capacity() % TOTAL_LEVEL;
	}

	/**
	 * To get the capacity of the concrete subclass
	 * @return Bag capacity, in number of Items allowed
	 */
	protected int capacity() {
		return capacity;
	}
	
//	public void setCapacity(int newCapacity) {
//		//TODO resize arrays, etc...
//	}

	/**
	 * Get the item decay rate, which differs in difference subclass, and can be 
	 * changed in run time by the user, so not a constant.
	 * @return The number of times for a decay factor to be fully applied
	 */
	abstract public int forgetRate();

	/**
	 * Get the average priority of Items
	 * @return The average priority of Items in the bag
	 */
	public float averagePriority() {
		if (nameTable.size() == 0) {
			return 0.01f;
		}
		float f = (float) mass / (nameTable.size() * TOTAL_LEVEL);
		if (f > 1) {
			return 1.0f;
		}
		return f;
	}

	/**
	 * Check if an item is in the bag
	 * @param it An item
	 * @return Whether the Item is in the Bag
	 */
	public boolean contains(Type it) {
		return nameTable.containsValue(it);
	}

	/**
	 * Get an Item by key
	 * @param key The key of the Item
	 * @return The Item with the given key
	 */
	public Type get(String key) {
		return nameTable.get(key);
	}

	/**
	 * Add a new Item into the Bag
	 * @param newItem The new Item
	 */
	public void putIn(Type newItem) {
		String newKey = newItem.getKey();
		Type oldItem = nameTable.put(newKey, newItem);
		if (oldItem != null) {                  // merge duplications
			outOfBase(oldItem);
			newItem.merge(oldItem);
		}
		Type overflowItem = intoBase(newItem);  // put the (new or merged) item into itemTable
		if (overflowItem != null) {             // remove overflow
			String overflowKey = overflowItem.getKey();
			nameTable.remove(overflowKey);
		}
	}

	/**
	 * Put an item back into the itemTable
	 * <p>
	 * The only place where the forgetting rate is applied
	 * @param oldItem The Item to put back
	 */
	public void putBack(Type oldItem) {
		BudgetFunctions.forget(oldItem.getBudget(), forgetRate(), RELATIVE_THRESHOLD);
		putIn(oldItem);
	}

	/**
	 * Choose an Item according to priority distribution and take it out of the Bag
	 * @return The selected Item
	 */
	public Type takeOut() {
		if (mass == 0) { // empty bag
			return null;
		}
		
		int levelIndex = getLevelIndex();
		
		if (emptyLevel(currentLevel) || (currentCounter == 0)) { // done with the current level
			currentLevel = DISTRIBUTOR.pick(levelIndex);
			levelIndex = DISTRIBUTOR.next(levelIndex);
			while (emptyLevel(currentLevel)) {          // look for a non-empty level
				currentLevel = DISTRIBUTOR.pick(levelIndex);
				levelIndex = DISTRIBUTOR.next(levelIndex);
			}
			if (currentLevel < THRESHOLD) { // for dormant levels, take one item
				currentCounter = 1;
			} else {                  // for active levels, take all current items
				currentCounter = itemTable[currentLevel].size();
			}
		}
		Type selected = takeOutFirst(currentLevel); // take out the first item in the level
		currentCounter--;
		nameTable.remove(selected.getKey());
		refresh();
		return selected;
	}

	/**
	 * Pick an item by key, then remove it from the bag
	 * @param key The given key
	 * @return The Item with the key
	 */
	public Type pickOut(String key) {
		Type picked = nameTable.get(key);
		if (picked != null) {
			outOfBase(picked);
			nameTable.remove(key);
		}
		return picked;
	}

	/**
	 * Check whether a level is empty
	 * @param n The level index
	 * @return Whether that level is empty
	 */
	private boolean emptyLevel(int n) {
		return ((itemTable[n] == null) || itemTable[n].isEmpty());
	}

	/**
	 * Decide the put-in level according to priority
	 * @param item The Item to put in
	 * @return The put-in level
	 */
	private int getLevel(Type item) {
		float fl = item.getPriority() * TOTAL_LEVEL;
		int level = (int) Math.ceil(fl) - 1;
		return (level < 0) ? 0 : level;     // cannot be -1
	}

	/**
	 * Insert an item into the itemTable, and return the overflow
	 * @param newItem The Item to put in
	 * @return The overflow Item
	 */
	@SuppressWarnings("unchecked")
	private Type intoBase(Type newItem) {
		Type oldItem = null;
		int inLevel = getLevel(newItem);
		if (nameTable.size() > capacity) {      // the bag is full
			int outLevel = 0;
			while (emptyLevel(outLevel)) {
				outLevel++;
			}
			if (outLevel > inLevel) {           // ignore the item and exit
				return newItem;
			} else {                            // remove an old item in the lowest non-empty level
				oldItem = takeOutFirst(outLevel);
			}
		}
		if (itemTable[inLevel] == null) {       // first time insert
			itemTable[inLevel] = new ArrayList();
		}
		itemTable[inLevel].add(newItem);        // FIFO
		mass += (inLevel + 1);                  // increase total mass

		refresh();                              // refresh the wondow

		return oldItem;
	}

	/**
	 * Take out the first or last Type in a level from the itemTable
	 * @param level The current level
	 * @return The first Item
	 */
	private Type takeOutFirst(int level) {
		Type selected = itemTable[level].get(0);
		itemTable[level].remove(0);
		mass -= (level + 1);
		refresh();
		return selected;
	}

	/**
	 * Remove an item from itemTable, then adjust mass
	 * @param oldItem The Item to be removed
	 */
	protected void outOfBase(Type oldItem) {
		int level = getLevel(oldItem);
		itemTable[level].remove(oldItem);
		mass -= (level + 1);
		refresh();
	}


	/**
	 * Collect Bag content into a String for display
	 */
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer(" ");
		int minLevel = 1;
		for (int i = TOTAL_LEVEL; i >= minLevel; i--) {
			if (!emptyLevel(i - 1)) {
				buf = buf.append("\n --- Level " + String.valueOf(i) + ":\n ");
				for (int j = 0; j < itemTable[i - 1].size(); j++) {
					buf = buf.append(itemTable[i - 1].get(j) + "\n ");
				}
			}
		}
		return buf.toString();
	}

	//  /**
	//  * To start displaying the Bag in a BagWindow
	//  * @param title The title of the window
	//  */
	// public void startPlay(String title) {
	//		window = new BagWindow(this, title);
	//     showing = true;
	//     window.post(toString());
	// }

	// /**
	//  * Resume display
	//  */
	// public void play() {
	//     showing = true;
	//     window.post(toString());
	// }
	//
	
	/**
	 * Refresh display
	 */
	public void refresh() {
//     if (showing) {
//         window.post(toString());
//     }
	}


}
