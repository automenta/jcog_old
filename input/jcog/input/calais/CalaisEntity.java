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
public class CalaisEntity implements Serializable, AtomType {
    public final String id;
    public final String name;
    public final String type;
    public final double relevance;

    public CalaisEntity(CalaisObject entity) {
        this.id = entity.getField("_uri");
        this.relevance = Double.parseDouble(entity.getField("relevance"));
        this.type = entity.getField("_type");
        this.name = entity.getField("name");
    }

    @Override
    public String toString() {
        return type + ":" + name + "(" + relevance + ")";
    }
    
    
}
