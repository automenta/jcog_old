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
public class CalaisTopic implements Serializable, AtomType {
    public final String name;
    public final double score;
    public final String id;

    public CalaisTopic(CalaisObject topic) {
        super();
        this.id = topic.getField("category");
        this.name = topic.getField("categoryName");
        this.score = Double.parseDouble(topic.getField("score"));
                
    }

    @Override
    public String toString() {
        return name + "(" + score + ")";
    }
    
    
    
}
