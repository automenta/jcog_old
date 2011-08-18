/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog.atom;

import java.util.Collection;
import java.util.Iterator;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.Operation;
import org.apache.commons.collections15.Predicate;

/**
 *
 * @author seh
 */
public interface ReadableAtomSpace {
    
    public Collection<Atom> getAtoms(Class<? extends AtomType> type, boolean includeSubtypes);
    public Atom getEdge(Class<? extends AtomType> type, Atom... members);

    public Collection<Atom> getVertices();    
    public Collection<Atom> getEdges();
        
    public Collection<Atom> getIncidentEdges(Atom vertex);
    public Collection<Atom> getIncidentVertices(Atom edge);

    public Class<? extends AtomType> getType(Atom a);
    public String getName(Atom a);
    public int getArity(Atom e);
    public boolean containsAtom(Atom a);
    
    public Iterator<Atom> iterateAtoms();
    public Iterator<Atom> iterateVertices();
    public Iterator<Atom> iterateEdges();


    /**
     *
     * @param predicate
     * @param op
     * @return true continue visiting (in a compound AtomSpace), false to prematurely terminate the visit
     */
    public boolean visitVertices(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op);

    /**
     *
     * @param predicate
     * @param op
     * @return true continue visiting (in a compound AtomSpace), false to prematurely terminate the visit
     */
    public boolean visitEdges(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op);
    
}
