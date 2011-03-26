package jcog.opencog;

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

    //public final List<ReadableAtomSpace> layers;
    
    private Map<Atom, TruthValue> truth;
    private Map<Atom, AttentionValue> attention;
    private List<MindAgent> agents = new LinkedList();  
	
//	private FloatMap activation; //???
//	private FloatMap importance;//???

    public OCMind(MemoryAtomSpace a) {
        super();
        this.atomspace = a;
        
        truth = new HashMap();
        attention = new HashMap();
    }
    
    public TruthValue getTruth(Atom a) {
        TruthValue t = truth.get(a);
        if (t == null) {
            t = getDefaultTruth(a);
            truth.put(a, t);
        }
        return t;
    }
        
    public AttentionValue getAttention(Atom a) {
        AttentionValue t = attention.get(a);
        if (t == null) {
            t = getDefaultAttention(a);
            attention.put(a, t);
        }
        return t;
    }
                   
    public TruthValue getDefaultTruth(Atom a) {
        return new SimpleTruthValue();
    }
    
    public AttentionValue getDefaultAttention(Atom a) {
        boolean disposable = true;
        OCType type = atomspace.getType(a);
        if (type.equals(Atom.Type)) {
            disposable = false;
        }
        return new AttentionValue(disposable);
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

    public void setSTIRange(short minSTISeen, short maxSTISeen) {
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
        return atomspace.getAtoms(type, includeSubtypes);
    }

    @Override
    public Atom getEdge(OCType type, Atom... members) {        
        return atomspace.getEdge(type, members);
    }
    
    @Override
    public OCType getType(Atom a) {
        return atomspace.getType(a);
    }

    @Override
    public String getName(Atom a) {
        return atomspace.getName(a);
    }

    @Override
    public int getArity(Atom e) {
        return atomspace.getArity(e);
    }

    @Override
    public boolean hasAtom(Atom a) {
        return atomspace.hasAtom(a);
    }

    @Override
    public Iterator<Atom> iterateVertices() {
        return atomspace.iterateVertices();
    }

    @Override
    public Iterator<Atom> iterateEdges() {
        return atomspace.iterateEdges();
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

}
