/*
 * TaskLink.java
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

import org.opencog.reason.nars.language.Term;



/**
 * Reference to a Task.
 * <p>
 * The reason to separate a Task and a TaskLink is that the same Task can be linked from 
 * multiple Concepts, with different BudgetValue.
 */
public class TaskLink extends TermLink {

    /** The Task linked. The "target" field in TermLink is not used here. */
    private Task targetTask;
    /** Remember the TermLinks that has been used recently with this TaskLink */
    private ArrayList<String> record;
	private int recordLength;

    /**
     * Constructor
     * <p>
     * only called in Memory.continuedProcess
     * @param t The target Task
     * @param template The TermLink template
     * @param v The budget
     */
    public TaskLink(Task t, TermLink template, BudgetValue v, int recordLength) {
        super(v);
        
        this.recordLength = recordLength;
        
        targetTask = t;
        if (template == null) {
            type = TermLink.SELF;
            index = null;
        } else {
            type = template.getType();
            index = template.getIndices();
        }
        record = new ArrayList<String>(getRecordLength());
        setKey();   // as defined in TermLink
        key += t.getKey();
    }

    /**
     * Get the target Task
     * @return The linked Task
     */
    public Task getTargetTask() {
        return targetTask;
    }

    /**
     * Get the TermLink record
     * @return The list of TermLinks recently used
     */
    ArrayList<String> getRecord() {
        return record;
    }

    /**
     * Merge one TaskLink into another
     * @param that The other TaskLink
     */
    public void merge(Item that) {
        super.merge(that);                              // merge the budgets
        ArrayList<String> v = ((TaskLink) that).getRecord();
        for (int i = 0; i < v.size(); i++) {            // merge the records
            if (record.size() <= getRecordLength()) {
                record.add(v.get(i));
            }
        }
    }

    /**
     * To check whether a TaskLink should use a TermLink, return false if they 
     * interacted recently
     * <p>
     * called in TermLinkBag only
     * @param termLink The TermLink to be checked
     * @return Whether they are novel to each other
     */
    public boolean novel(TermLink termLink) {
        Term bTerm = termLink.getTarget();
        if (bTerm.equals(targetTask.getSentence().getContent())) {
            return false;
        }
        String linkKey = termLink.getKey();
        for (String recorded : record) {
            if (linkKey.equals(recorded)) {
                return false;
            }
        }
        record.add(linkKey);       // add knowledge reference to record
        if (record.size() > getRecordLength()) { // keep a constant length
            record.remove(0);
        }
        return true;
    }
    
    public int getRecordLength() {
    	return recordLength;
    }
}

