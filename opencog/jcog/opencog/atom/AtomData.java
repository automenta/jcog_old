/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.atom;

import jcog.opencog.AtomType;

/**
 *
 * @author seh
 */
public class AtomData {
    public final Class<? extends AtomType> type;
    public String name;
    public final TruthValue truth;
    public final AttentionValue attention;

    public AtomData(Class<? extends AtomType> type, String name, TruthValue truth, AttentionValue attention) {
        this.type = type;
        this.name = name;
        this.truth = truth;
        this.attention = attention;
    }
    
}
