/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.util;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import jcog.opencog.AtomType;

/**
 *
 * @author seh
 */
public class AtomTypes {

    private static List<Class<? extends AtomType>> _types = new LinkedList();
    public static List<Class<? extends AtomType>> types;

    static {
        for (Field f : AtomType.class.getFields()) {
            try {
                _types.add((Class<? extends AtomType>)f.get(null));
            } catch (IllegalArgumentException ex) {
                //ex.printStackTrace();
            } catch (IllegalAccessException ex) {
                //ex.printStackTrace();
            }
        }
        types = Collections.unmodifiableList(_types);
    }
    
    public static List<Class<? extends AtomType>> getTypes() {
        return types;
    }
    
}
