/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog;

import java.util.Collection;
import java.util.Iterator;

/**
 *
 * @author seh
 */
interface ReadableAtomSpace {
    
    public Collection<Atom> getAtoms(OCType type, boolean includeSubtypes);
    public Atom getEdge(OCType type, Atom... members);
    
    public Collection<Atom> getIncidentEdges(Atom vertex);
    public Collection<Atom> getIncidentVertices(Atom edge);

    public OCType getType(Atom a);
    public String getName(Atom a);
    public int getArity(Atom e);
    public boolean hasAtom(Atom a);
    public Iterator<Atom> iterateVertices();
    public Iterator<Atom> iterateEdges();

}
