/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input.calais;

import java.io.Serializable;
import jcog.opencog.AtomType;
import mx.bigdata.jcalais.CalaisObject;

/**
 *
 * @author seh
 */
public class CalaisTag implements Serializable, AtomType {
    public final String name;
    public final double importance;
    public final String id;

    public CalaisTag(CalaisObject tag) {
        this.name = tag.getField("name");
        this.id = tag.getField("id");
        this.importance = Double.parseDouble(tag.getField("importance"));
    }

    @Override
    public String toString() {
        return name + "(" + importance + ")";
    }
    
}
