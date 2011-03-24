package jcog.opencog;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import jcog.opencog.atom.AttentionValue;
import jcog.opencog.atom.SimpleTruthValue;
import jcog.opencog.atom.TruthValue;


public class OCMind implements ReadableAtomSpace {
    public final MemoryAtomSpace atomspace;

    private Map<Atom, TruthValue> truth;
    private Map<Atom, AttentionValue> attention;
    private List<MindAgent> activeAgents = new LinkedList();  
	
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

    public List<MindAgent> getAgentsActive() {
        return activeAgents;
    }

    @Override
    public Collection<Atom> getAtoms(OCType type, boolean includeSubtypes) {
        return atomspace.getAtoms(type, includeSubtypes);
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

}
