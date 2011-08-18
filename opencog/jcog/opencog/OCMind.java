package jcog.opencog;

import jcog.opencog.atom.ReadableAtomSpace;
import jcog.opencog.atom.EditableAtomSpace;
import jcog.opencog.atom.MemoryAtomSpace;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import jcog.opencog.atom.AtomData;
import jcog.opencog.attention.UpdateImportance;
import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;
import org.apache.commons.collections15.iterators.FilterIterator;
import org.apache.log4j.Logger;

/** Analogous to CogServer.
An atomspace implementation that interfaces to other embedded atomspace implementations.
 */
public class OCMind extends MemoryAtomSpace implements ReadableAtomSpace, EditableAtomSpace /* ReadableAttention, EditableAttention */ {

    final static Logger logger = Logger.getLogger(OCMind.class);
    
    //private final List<ReadableAtomSpace> subspaces;
    
    
    private List<MindAgent> agents = new CopyOnWriteArrayList();
    private long lastCycle = 0, currentCycle = 0;
    
    private final UpdateImportance updateImportance = new UpdateImportance();

    //Parallel mode: runs each cycle's MindAgent's in parallel -- NOT TESTED YET
    boolean parallel = false;

    public OCMind() {
        super();
    }


    @Override
    public boolean visitEdges(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        return visitEdges(predicate, op);
    }

    @Override
    public boolean visitVertices(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        return visitVertices(predicate, op);
    }
    

//	public FloatMap getActivation() {
//		return activation;
//	}
//
//	public FloatMap getImportance() {
//		return importance;
//	}
//	

    public MindAgent addAgent(MindAgent m) {
        if (agents.contains(m)) {
            logger.error("Can not add duplicate MindAgent " + m + " to " + this);
            return m;
        }
        agents.add(m);
        return m;
    }

    public boolean removeAgent(MindAgent m) {
        return agents.remove(m);
    }

    public double getCycleDT() {
        return ((double) (currentCycle - lastCycle)) / 1.0e9;
    }


    public void cycle() {
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
            for (final Atom v : ma.getVerticesToRemove()) {
                remove(v);
            }
            for (final Atom e : ma.getEdgesToRemove()) {
                remove(e);
            }
            ma.getVerticesToRemove().clear();
            ma.getEdgesToRemove().clear();
        }

        updateIndexes();
        
    }


    public List<MindAgent> getAgents() {
        return agents;
    }

    public void printAtoms() {
        Iterator<Atom> i = iterateAtoms();
        while (i.hasNext()) {
            Atom a = i.next();
            System.out.println("  " + a.toString() + " : " + getType(a).getSimpleName() + " : " + getName(a) + " "
                    + getIncidentEdgeDegree(a) + "|" + getIncidentVertexDegree(a));
        }
    }

    public MindRunner start(double period) {
        return new MindRunner(this, period, true);
    }
    public MindRunner run(double period) {
        return new MindRunner(this, period, false);
    }

    public boolean setName(Atom a, String newName) {
        return setName(a, newName);
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


    public double getSecondsSinceLastCycle() {
        final long current = System.nanoTime();
        return ((double) (current - currentCycle)) / 1.0e9;
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
        List<Atom> a = new LinkedList();

        Iterator<Atom> ia = iterateAtoms();
        while (ia.hasNext()) {
            Atom x = ia.next();
            AtomData ad = getData(x);
            if (ad.name.contains(substring))
                a.add(x);
        }

        return Collections.unmodifiableList(a);
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
