/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog;

/**
 *
 * @author seh
 */
interface EditableAtomSpace extends ReadableAtomSpace {
    
    Atom addEdge(OCType t, Atom... members);

    //boolean addVertex(OCType type, Atom a);

    boolean addVertex(OCType type, Atom a, String name);

    //Atom addVertex(OCType type, String name);

    void clear();

    boolean removeEdge(Atom e);

    boolean removeVertex(Atom a);

}
