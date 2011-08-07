/*
 * Task.java
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

import jcog.nars.reason.language.Term;


/**
 * A task to be processed, consists of a Sentence and a BudgetValue
 */
public class Task extends Item {
    /** The sentence of the Task */
    private Sentence sentence;
    /** Whether it is derived by a structual rule */
    protected boolean structual = false;        // 

    /**
     * Constructor
     * @param s The sentence
     * @param b The budget
     */
    public Task(Sentence s, BudgetValue b) {
        super(b);
        sentence = s;
        key = sentence.toKey();
    }

    /**
     * Get the sentence
     * @return The sentence
     */
    public Sentence getSentence() {
        return sentence;
    }

    /**
     * Directly get the content of the sentence
     * @return The content of the sentence
     */
    public Term getContent() {
        return sentence.getContent();
    }

    /**
     * Directly get the tense of the sentence
     * @return The tense of the sentence
     */
    public TemporalValue getTense() {
        return sentence.getTense();
    }

    /**
     * Check if a Task is derived by a StructuralRule
     * @return Whether the Task is derived by a StructuralRule
     */
    public boolean isStructual() {
        return structual;
    }

    /**
     * Record if a Task is derived by a StructuralRule
     */
    public void setStructual() {
        structual = true;
    }

    /**
     * Merge one Task into another
     * @param that The other Task
     */
    public void merge(Item that) {
        super.merge(that);
        structual = (structual || ((Task) that).isStructual());
    }

    /**
     * Get a String representation of the Task
     * @return The Task as a String
     */
    @Override
    public String toString() {
        StringBuffer s = new StringBuffer();
        s.append(sentence);
        return s.toString();
    }

    /**
     * Get a String representation of the Task, with reduced accuracy
     * @return The Task as a String, with 2-digit accuracy for the values
     */
    @Override
    public String toString2() {
        StringBuffer s = new StringBuffer();
        if (sentence instanceof Question) {
            s.append(sentence);
        } else {
            s.append(((Judgment) sentence).toString());
        }
        return s.toString();
    }
}
