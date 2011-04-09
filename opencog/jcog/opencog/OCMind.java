package jcog.opencog;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import edu.uci.ics.jung.graph.util.Pair;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import jcog.opencog.atom.AttentionValue;
import jcog.opencog.atom.SimpleTruthValue;
import jcog.opencog.atom.TruthValue;
import org.apache.log4j.Logger;

/** Analogous to CogServer.
    An atomspace implementation that interfaces to other embedded atomspace implementations.
 */
public class OCMind implements ReadableAtomSpace, EditableAtomSpace /* ReadableAttention, EditableAttention */ {
    final static Logger logger = Logger.getLogger(OCMind.class);
    
    /** the default in-memory store */
    public final MemoryAtomSpace atomspace;

    public final List<ReadableAtomSpace> subspaces;
    
    private Map<Atom, TruthValue> truth;
    private Map<Atom, AttentionValue> attention;
    private List<MindAgent> agents = new LinkedList();
    private short minSTISeen = 0, maxSTISeen = 0;
	
//	private FloatMap activation; //???
//	private FloatMap importance;//???

    public OCMind(MemoryAtomSpace a) {
        super();
        this.atomspace = a;
        
        subspaces = new LinkedList();
        
        truth = new HashMap();
        attention = new HashMap();
    }
    
    public TruthValue getTruth(Atom a) {
        TruthValue t = truth.get(a);
        if (t == null) {
            t = newDefaultTruthValue(a);
            truth.put(a, t);
        }
        return t;
    }
        
    public AttentionValue getAttention(Atom a) {
        AttentionValue t = attention.get(a);
        if (t == null) {
            t = newDefaultAttentionValue(a);
            attention.put(a, t);
        }
        return t;
    }
                   
    public TruthValue newDefaultTruthValue(Atom a) {
        return new SimpleTruthValue();
    }
    
    public AttentionValue newDefaultAttentionValue(Atom a) {
        boolean disposable = true;
        OCType type = atomspace.getType(a);
        if (type.equals(Atom.Type)) {
            disposable = false;
        }
        return new AttentionValue(disposable);
    }

    @Override
    public boolean visitEdges(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        return atomspace.visitEdges(predicate, op);
    }

    @Override
    public boolean visitVertices(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        return atomspace.visitVertices(predicate, op);
    }


    @Override
    public Atom addEdge(OCType t, Atom... members) {
        return atomspace.addEdge(t, members);
    }

    @Override
    public boolean addVertex(OCType type, Atom a) {
        return atomspace.addVertex(type, a);
    }

    @Override
    public boolean addVertex(OCType type, Atom a, String name) {
        return atomspace.addVertex(type, a, name);
    }

    @Override
    public Atom addVertex(OCType type, String name) {
        return atomspace.addVertex(type, name);
    }

    @Override
    public void clear() {
        atomspace.clear();
    }

    @Override
    public boolean removeEdge(Atom e) {
        return atomspace.removeEdge(e);
    }

    @Override
    public boolean removeVertex(Atom a) {
        return atomspace.removeVertex(a);
    }



//	public FloatMap getActivation() {
//		return activation;
//	}
//
//	public FloatMap getImportance() {
//		return importance;
//	}
//	

    /** called by UpdateImportance -- indicates the min and max STI seen by update importance as it iterates across all atoms utilized by Agents */
    public void setSTIRange(short minSTISeen, short maxSTISeen) {
        this.minSTISeen = minSTISeen;
        this.maxSTISeen = maxSTISeen;
    }

    public void addAgent(MindAgent m) {
        if (agents.contains(m)) {
            logger.error("Can not add duplicate MindAgent " + m + " to " + this);
            return;
        }
        agents.add(m);
    }
    
    public List<MindAgent> getAgents() {
        return agents;
    }

    @Override
    public Collection<Atom> getAtoms(OCType type, boolean includeSubtypes) {
        Builder<Atom> ib = new ImmutableList.Builder<Atom>();
        ib.addAll(atomspace.getAtoms(type, includeSubtypes));
        for (ReadableAtomSpace ra : subspaces) {
            //TODO add parameter so that ra.getAtoms can use this (parent)'s type hierarchy
            ib.addAll(ra.getAtoms(type, includeSubtypes));
        }
        return ib.build();
    }
    
    @Override
    public Collection<Atom> getVertices() {
        Builder<Atom> ib = new ImmutableList.Builder<Atom>();
        ib.addAll(atomspace.getVertices());
        for (ReadableAtomSpace ra : subspaces) {
            ib.addAll(ra.getVertices());
        }        
        return ib.build();
    }

    @Override
    public Collection<Atom> getEdges() {
        Builder<Atom> ib = new ImmutableList.Builder<Atom>();
        ib.addAll(atomspace.getEdges());
        for (ReadableAtomSpace ra : subspaces) {
            ib.addAll(ra.getEdges());
        }        
        return ib.build();
    }


    @Override
    public Atom getEdge(OCType type, Atom... members) {        
        Atom a = atomspace.getEdge(type, members);
        if (a != null) {
            return a;
        }
        
        for (ReadableAtomSpace ra : subspaces) {
            Atom sa = ra.getEdge(type, members);
            if (sa!=null)
                return sa;
        }
        return null;
    }
    
    @Override
    public OCType getType(Atom a) {
        OCType t = atomspace.getType(a);
        if (t!=null)
            return t;
        //TODO look in subgraphs
        return null;
    }

    @Override
    public String getName(Atom a) {
        String n = atomspace.getName(a);
        if (n!=null)
            return n;
        return null;
        //TODO look in subgraphs
    }

    @Override
    public int getArity(Atom e) {
        return atomspace.getArity(e);
        //TODO look in subgraphs
    }

    @Override
    public boolean hasAtom(Atom a) {
        return atomspace.hasAtom(a);
        //TODO look in subgraphs
    }

    @Override
    public Iterator<Atom> iterateVertices() {
        return atomspace.iterateVertices();
        //TODO look in subgraphs
    }

    @Override
    public Iterator<Atom> iterateEdges() {
        return atomspace.iterateEdges();
        //TODO look in subgraphs
    }


    public void setVLTI(Atom a, int newVLTI) {
        getAttention(a).setVLTI(newVLTI);
    }

    public short getSTI(Atom a) {
        return getAttention(a).getSTI();
    }

    public short getLTI(Atom a) {
        return getAttention(a).getLTI();
    }

    /** returns an atom's STI normalized to -1..+1 range */
    public double getNormalizedSTI(Atom a) {
        double sti = getSTI(a);
        
        if (maxSTISeen!=minSTISeen)
            return (sti - ((double)minSTISeen)) / (double)(maxSTISeen - minSTISeen);
        else
            return 0;
    }
    
    public Pair<Short> getSTIRange(Collection<Atom> atoms) {
        boolean first = true;
                
        short minSTI=0, maxSTI=0;
        
        for (Atom x : atoms) {
            if (first) {
                minSTI = maxSTI = getSTI(x);                
                first = false;
            }
            else {
                short s = getSTI(x);
                if (s < minSTI)
                    minSTI = s;
                if (s > maxSTI)
                    maxSTI = s;            
            }
        }
        
        return new Pair<Short>(minSTI, maxSTI);
    }

    @Override
    public Collection<Atom> getIncidentEdges(Atom vertex) {
        return atomspace.getIncidentEdges(vertex);
        //TODO look in subgraphs
    }

    @Override
    public Collection<Atom> getIncidentVertices(Atom edge) {
        return atomspace.getIncidentVertices(edge);    
        //TODO look in subgraphs
    }

    public short getMaxSeenSTI() {
        return maxSTISeen;
    }
    public short getMinSeenSTI() {
        return minSTISeen;
    }

    
    
}
