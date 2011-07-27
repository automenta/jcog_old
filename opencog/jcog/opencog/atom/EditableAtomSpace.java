/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog.atom;

import jcog.opencog.Atom;
import jcog.opencog.AtomType;

/**
 *
 * @author seh
 */
public interface EditableAtomSpace extends ReadableAtomSpace {
    
    Atom addEdge(Class<? extends AtomType> t, String name, Atom... members);

    boolean addVertex(Class<? extends AtomType> type, Atom a, String name);

    void clear();

    boolean remove(Atom e);

}
