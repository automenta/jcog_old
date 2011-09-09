/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.atom.dann;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.syncleus.dann.graph.MutableHyperAdjacencyGraph;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NavigableMap;
import java.util.logging.Logger;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.Operation;
import jcog.opencog.atom.AtomData;
import jcog.opencog.atom.AttentionValue;
import jcog.opencog.atom.ReadableAtomSpace;
import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;

/**
 *
 * @author seh
 */
public class MemoryAtomSpaceDANN2 extends MutableHyperAdjacencyGraph<Atom, AtomEdge> implements ReadableAtomSpace {
    
    final static Logger logger = Logger.getLogger(MemoryAtomSpaceDANN2.class.toString());
        
    private final Map<Atom, AtomData> atomData = new HashMap();
    private Multimap<Class<? extends AtomType>, Atom> typesToAtom = HashMultimap.create();
    protected NavigableMap<Atom, AttentionValue> attentionSortedBySTI;
    private short minSTISeen = 0, maxSTISeen = 0;

    public MemoryAtomSpaceDANN2() {
        super();
    }
    
    @Override
    public Collection<Atom> getAtoms(Class<? extends AtomType> type, boolean includeSubtypes) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Atom getEdge(Class<? extends AtomType> type, Atom... members) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Collection<Atom> getVertices() {
        return getNodes();
    }


    @Override
    public Collection<? extends Atom> getIncidentEdges(Atom vertex) {
        return getAdjacentEdges(vertex);
    }

    @Override
    public Collection<Atom> getIncidentVertices(Atom edge) {
        return edge.getNodes();
    }

    @Override
    public Class<? extends AtomType> getType(Atom a) {
        return atomData.get(a).type;
    }

    @Override
    public String getName(Atom a) {
        return atomData.get(a).name;
    }
    

    @Override
    public int getArity(Atom e) {
        return e.getDegree();
    }

    @Override
    public boolean containsAtom(Atom a) {
        return atomData.containsKey(a);
    }

    @Override
    public Iterator<Atom> iterateAtoms() {        
        return IteratorUtils.chainedIterator(iterateVertices(), (Iterator<Atom>)iterateEdges());
    }

    @Override
    public Iterator<Atom> iterateVertices() {
        return getVertices().iterator();
    }

    @Override
    public Iterator<? extends Atom> iterateEdges() {
        return getEdges().iterator();
    }

    @Override
    public boolean visitVertices(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public boolean visitEdges(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
        
    
}
