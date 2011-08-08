/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.atom;

import java.util.HashMap;
import java.util.Map;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.OCMind;

/**
 *
 * @author seh
 */
public class MapToAtom<X> {
    private Map<X, Atom> m = new HashMap();
    private final OCMind mind;
    private final Class<? extends AtomType> type;
    boolean useToString = true;

    public MapToAtom(Class<? extends AtomType> type, OCMind mind) {
        this.type = type;
        this.mind = mind;
    }

    public Atom get(X x) {
        Atom a = m.get(x);
        if (a == null) {
            if (useToString) {
                a = mind.addVertex(type, x.toString());
            } else {
                a = mind.addVertex(type);
            }
            m.put(x, a);
        }
        return a;
    }

    public boolean has(X x) {
        return m.containsKey(x);
    }
    
}
