package jcog.opencog;

import jcog.opencog.atom.ReadableAtomSpace;
import jcog.opencog.atom.EditableAtomSpace;
import jcog.opencog.atom.MemoryAtomSpace;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import edu.stanford.nlp.util.CollectionUtils;
import edu.uci.ics.jung.graph.util.Pair;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import jcog.opencog.atom.AttentionValue;
import jcog.opencog.atom.SimpleTruthValue;
import jcog.opencog.atom.TruthValue;
import jcog.opencog.attention.UpdateImportance;
import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;
import org.apache.commons.collections15.iterators.FilterIterator;
import org.apache.log4j.Logger;

/** Analogous to CogServer.
An atomspace implementation that interfaces to other embedded atomspace implementations.
 */
public class OCMind implements ReadableAtomSpace, EditableAtomSpace /* ReadableAttention, EditableAttention */ {

    final static Logger logger = Logger.getLogger(OCMind.class);
    
    private final MemoryAtomSpace atomspace; /// the default in-memory store
    
    private final List<ReadableAtomSpace> subspaces;
    private Map<Atom, TruthValue> truth;
    private Map<Atom, AttentionValue> attention;
    private TreeMap<Atom, AttentionValue> attentionSortedBySTI;
    private List<MindAgent> agents = new CopyOnWriteArrayList();
    private short minSTISeen = 0, maxSTISeen = 0;
    private long lastCycle = 0, currentCycle = 0;
    private UpdateImportance updateImportance = new UpdateImportance();

    //Parallel mode: runs each cycle's MindAgent's in parallel -- NOT TESTED YET
    boolean parallel = false;

    public OCMind() {
        this(new MemoryAtomSpace());
    }

    public OCMind(MemoryAtomSpace a) {
        super();
        this.atomspace = a;

        subspaces = new LinkedList();

        truth = new HashMap();
        attention = new HashMap();
                
        updateAttentionSort();

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
        Class<? extends AtomType> type = atomspace.getType(a);
        return new AttentionValue(true);
    }

    @Override
    public boolean visitEdges(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        return atomspace.visitEdges(predicate, op);
    }

    @Override
    public boolean visitVertices(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        return atomspace.visitVertices(predicate, op);
    }
    
    public Atom addEdge(Class<? extends AtomType> t, List<Atom> members) {
        Atom[] ma = new Atom[members.size()];
        members.toArray(ma);    
        return addEdge(t, ma);
    }

    public Atom addEdge(Class<? extends AtomType> t, String name, Atom... members) {
        Atom e = atomspace.addEdge(t, name, members);
        if (e != null) {
            //getAttention(e);
        }
        return e;
    }

    public Atom addEdge(Class<? extends AtomType> t, Atom... members) {
        return addEdge(t, null, members);
    }

    @Override
    public boolean addVertex(Class<? extends AtomType> type, Atom a, String name) {
        if (atomspace.addVertex(type, a, name)) {
            //getAttention(a);
            return true;
        }
        return false;
    }

    public Atom addVertex(Class<? extends AtomType> type) {
        final Atom a = new Atom();
        if (addVertex(type, a, a.toString())) {
            return a;
        }
        return null;
    }

    public Atom addVertex(Class<? extends AtomType> type, String name) {
        final Atom a = new Atom();
        if (addVertex(type, a, name)) {
            return a;
        }
        return null;
    }

    @Override
    public void clear() {
        atomspace.clear();
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

    public synchronized MindAgent addAgent(MindAgent m) {
        if (agents.contains(m)) {
            logger.error("Can not add duplicate MindAgent " + m + " to " + this);
            return m;
        }
        agents.add(m);
        return m;
    }

    public synchronized boolean removeAgent(MindAgent m) {
        return agents.remove(m);
    }

    public double getCycleDT() {
        return ((double) (currentCycle - lastCycle)) / 1.0e9;
    }

    public synchronized void updateAttentionSort() {
        //cleanup attention
        List<Atom> toRemoveFromAttentionAndTruth = new LinkedList();

        for (Atom a : attention.keySet()) {
            if (!atomspace.hasAtom(a)) {
                toRemoveFromAttentionAndTruth.add(a);
            }
        }

        for (Atom a : toRemoveFromAttentionAndTruth) {
            attention.remove(a);
            truth.remove(a);
        }


            attentionSortedBySTI = new TreeMap<Atom, AttentionValue>(new Comparator<Atom>() {

                @Override
                public int compare(Atom a, Atom b) {
                    short sa = getSTI(a);
                    short sb = getSTI(b);

                    if (sa == sb) {
                        return -1;
                    }
                    return (sa > sb) ? -1 : 1;
                }
            });
        synchronized (attentionSortedBySTI) {
            attentionSortedBySTI.putAll(attention);
        }
//        final Iterator<Atom> i = iterateAtoms();
//        final AttentionValue zv = new AttentionValue(Short.MIN_VALUE);
//        while (i.hasNext()) {
//            Atom a = i.next();
//            if (!attention.containsKey(a))
//                attentionSortedBySTI.put(a, zv);
//        }
    }

    public synchronized void cycle() {
        lastCycle = currentCycle;
        currentCycle = System.nanoTime();

        if (parallel) {
            final ExecutorService threadPool = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
            final double dt = getCycleDT();
            for (final MindAgent ma : agents) {
                threadPool.submit(new Runnable() {
                    @Override
                    public void run() {
                        ma._run(OCMind.this, dt);
                    }
                });
            }
            threadPool.shutdown();
            try {
                threadPool.awaitTermination(1, TimeUnit.MINUTES);
            } catch (InterruptedException ex) {
                java.util.logging.Logger.getLogger(OCMind.class.getName()).log(Level.SEVERE, null, ex);
            }
        } else {
            final double dt = getCycleDT();
            for (final MindAgent ma : agents) {
                ma._run(OCMind.this, dt);
            }
        }

        updateImportance.update(this);

        //TODO remove removals before updateImportance to avoid updating importance on removed atoms

        //remove all pending removals
        for (final MindAgent ma : agents) {
            for (Atom v : ma.getVerticesToRemove()) {
                remove(v);
            }
            for (Atom e : ma.getEdgesToRemove()) {
                remove(e);
            }
            ma.getVerticesToRemove().clear();
            ma.getEdgesToRemove().clear();
        }

        updateAttentionSort();
        
    }


    public List<MindAgent> getAgents() {
        return agents;
    }

    public boolean isVertex(Atom a) {
        return atomspace.isVertex(a);
    }

    public int getVertexCount() {
        return atomspace.getVertices().size();
    }

    public int getEdgeCount() {
        return atomspace.getEdges().size();
    }

    public void printAtoms() {
        Iterator<Atom> i = iterateAtoms();
        while (i.hasNext()) {
            Atom a = i.next();
            System.out.println("  " + a.toString() + " : " + getType(a).getSimpleName() + " : " + getName(a) + " "
                    + atomspace.getIncidentEdgeDegree(a) + "|" + atomspace.getIncidentVertexDegree(a));
        }
    }

    public MindRunner start(double period) {
        return new MindRunner(this, period);
    }

    public boolean setName(Atom a, String newName) {
        return atomspace.setName(a, newName);
    }

    public class AtomTypeArityPredicate implements org.apache.commons.collections15.Predicate<Atom> {

        private final Class<? extends AtomType> type;
        private final boolean includeSubtypes;
        private final int minArity;
        private final int maxArity;

        public AtomTypeArityPredicate(Class<? extends AtomType> type, boolean includeSubtypes) {
            this(type, includeSubtypes, -1, -1);
        }

        /**
         * @param type if null, disables comparison
         * @param includeSubtypes if false, only accepts exact type class; otherwise accepts subclasses
         * @param minArity  inclusive minimum accepted arity, or -1 to disable comparison
         * @param maxArity  inclusive maximum accepted arity, or -1 to disable comparison
         */
        public AtomTypeArityPredicate(Class<? extends AtomType> type, boolean includeSubtypes, int minArity, int maxArity) {
            this.type = type;
            this.includeSubtypes = includeSubtypes;
            this.minArity = minArity;
            this.maxArity = maxArity;
        }

        @Override
        public boolean evaluate(Atom x) {
            if (type != null) {
                final Class<? extends AtomType> t = getType(x);
                if (includeSubtypes) {
                    if (!type.isAssignableFrom(t)) {
                        return false;
                    }
                } else {
                    if (!t.equals(type)) {
                        return false;
                    }
                }
            }

            final int arity = getArity(x);
            if (minArity != -1) {
                if (arity < minArity) {
                    return false;
                }
            }
            if (maxArity != -1) {
                if (arity > maxArity) {
                    return false;
                }
            }
            return true;
        }
    }

    public Iterator<Atom> iterateAtoms(Class<? extends AtomType> type, boolean includeSubtypes) {
        return iterateAtoms(type, includeSubtypes, -1, -1);
    }

    /**
     * 
     * @param type if null, disables comparison
     * @param includeSubtypes if false, only accepts exact type class; otherwise accepts subclasses
     * @param minArity  inclusive minimum accepted arity, or -1 to disable comparison
     * @param maxArity  inclusive maximum accepted arity, or -1 to disable comparison
     * @return 
     */
    public Iterator<Atom> iterateAtoms(Class<? extends AtomType> type, boolean includeSubtypes, int minArity, int maxArity) {
        return new FilterIterator<Atom>(iterateAtoms(), new AtomTypeArityPredicate(type, includeSubtypes, minArity, maxArity));
    }

    public Iterator<Atom> iterateEdges(Class<? extends AtomType> type, boolean includeSubtypes, int minArity, int maxArity) {
        return new FilterIterator<Atom>(iterateEdges(), new AtomTypeArityPredicate(type, includeSubtypes, minArity, maxArity));
    }

    @Override
    public List<Atom> getAtoms(Class<? extends AtomType> type, boolean includeSubtypes) {
        Builder<Atom> ib = new ImmutableList.Builder<Atom>();
        Collection<Atom> v = atomspace.getAtoms(type, includeSubtypes);
        if (v != null) {
            ib.addAll(v);
            for (ReadableAtomSpace ra : subspaces) {
                //TODO add parameter so that ra.getAtoms can use this (parent)'s type hierarchy
                ib.addAll(ra.getAtoms(type, includeSubtypes));
            }
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
    public Atom getEdge(Class<? extends AtomType> type, Atom... members) {
        Atom a = atomspace.getEdge(type, members);
        if (a != null) {
            return a;
        }

        for (ReadableAtomSpace ra : subspaces) {
            Atom sa = ra.getEdge(type, members);
            if (sa != null) {
                return sa;
            }
        }
        return null;
    }

    @Override
    public Class<? extends AtomType> getType(final Atom a) {
        Class<? extends AtomType> t = atomspace.getType(a);
        if (t != null) {
            return t;
        }
        //TODO look in subgraphs
        return null;
    }

    @Override
    public String getName(final Atom a) {
        String n = atomspace.getName(a);
        if (n != null) {
            return n;
        }
        return null;
        //TODO look in subgraphs
    }

    @Override
    public int getArity(final Atom e) {
        return atomspace.getArity(e);
        //TODO look in subgraphs
    }

    @Override
    public boolean hasAtom(Atom a) {
        return atomspace.hasAtom(a);
        //TODO look in subgraphs
    }

    @Override
    public Iterator<Atom> iterateAtoms() {
        return atomspace.iterateAtoms();
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

    public double getNormalizedSTI(Atom a, double maxSTI, double minSTI) {
        double sti = getSTI(a);

        if (maxSTI != minSTI) {
            return (sti - ((double) minSTI)) / (double) (maxSTI - minSTI);
        } else {
            return 0;
        }
    }

    /** returns an atom's STI normalized to -1..+1 range */
    public double getNormalizedSTI(Atom a) {
        return getNormalizedSTI(a, maxSTISeen, minSTISeen);
    }

    public Pair<Short> getSTIRange(Collection<Atom> atoms) {
        boolean first = true;

        short minSTI = 0, maxSTI = 0;

        for (Atom x : atoms) {
            if (first) {
                minSTI = maxSTI = getSTI(x);
                first = false;
            } else {
                short s = getSTI(x);
                if (s < minSTI) {
                    minSTI = s;
                }
                if (s > maxSTI) {
                    maxSTI = s;
                }
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

    public double getSecondsSinceLastCycle() {
        final long current = System.nanoTime();
        return ((double) (current - currentCycle)) / 1.0e9;
    }

    //TODO debug this
    public int getOrderInIncidentEdge(Atom a, Atom edge) {
        int i = 0;
        if (getIncidentEdges(edge) == null) {
            return -1;
        }
        for (Atom p : getIncidentEdges(edge)) {
            if (p.equals(a)) {
                return i;
            }
            i++;
        }
        return -1;
    }

    public void printAtom(Atom a) {
        System.out.println("  " + a.toString() + ": " + getName(a) + " " + getType(a) + "\n");
    }

    public void printAtomNeighborhood(Atom y) {
        System.out.println(y + " " + getName(y));
        System.out.println(" Incident Edges:");
        if (getIncidentEdges(y) != null) {
            for (Atom e : getIncidentEdges(y)) {
                //System.out.print(getOrderInIncidentEdge(y, e) + " ");
                printAtom(e);
            }
        }
        if (getIncidentVertices(y) != null) {
            System.out.println(" Incident Vertices:");
            for (Atom e : getIncidentVertices(y)) {
                printAtom(e);
            }
        }
    }

    public List<Atom> getAtomsByName(String substring) {
        List<Atom> a = new ArrayList();

        for (Entry<Atom, String> e : atomspace.getNameIndex().entrySet()) {
            if (e.getValue().contains(substring)) {
                a.add(e.getKey());
            }
        }

        return a;
    }

    public Iterator<Atom> iterateAtomsByDecreasingSTI() {
        return iterateAtomsBySTI(true, null);
    }

    public Iterator<Atom> iterateAtomsByDecreasingSTI(final Predicate<Atom> include) {
        return iterateAtomsBySTI(true, include);
    }

    public Iterator<Atom> iterateAtomsByIncreasingSTI() {
        return iterateAtomsBySTI(false, null);
    }

    public Iterator<Atom> iterateAtomsByIncreasingSTI(final Predicate<Atom> include) {
        return iterateAtomsBySTI(false, include);
    }

    /**
     * 
     * @param increasingOrDecreasing decreasing=true, increasing=false
     * @param include
     * @return 
     */
    public Iterator<Atom> iterateAtomsBySTI(final boolean increasingOrDecreasing, final Predicate<Atom> include) {
        synchronized (attentionSortedBySTI) {

            Iterator<Atom> i;
            if (increasingOrDecreasing) {
                i = attentionSortedBySTI.navigableKeySet().iterator();
            } else {
                i = attentionSortedBySTI.navigableKeySet().descendingIterator();
            }

            if (include != null) {
                return IteratorUtils.filteredIterator(i, include);
            } else {
                return i;
            }
        }
    }

    public boolean remove(Atom a) {
        if (atomspace.remove(a)) {
            return true;
        }
        return false;
    }

    public String getTypeName(final Atom a) {
        return getType(a).getSimpleName();
    }

    public MemoryAtomSpace getAtomSpace() {
        return atomspace;
    }
        
}
