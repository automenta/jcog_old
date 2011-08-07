/*
 * ConceptBag.java
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

import java.util.HashMap;
import jcog.nars.Concept;
import jcog.nars.reason.NARParams;

/**
 * Contains Concepts.
 */
public class ConceptBag extends Bag<Concept> {

    private NARParams param;

    /**
     * 
     * @param loadFactor
     * @param threshold
     * @param totalLevel
     * @param capacity			Parameters.CONCEPT_BAG_SIZE;
     * @param forgetRate		MainWindow.forgetCW.value();
     */
    public ConceptBag(NARParams param) {
        super(param.getBagLoadFactor(), param.getBagThreshold(), param.getBagLevel(), param.getConceptBagSize());
        this.param = param;
    }

    @Override
    public int forgetRate() {
        return param.getConceptForgetRate();
    }


    public HashMap<String, Concept> getNameTable() {
        return this.nameTable;
    }
}