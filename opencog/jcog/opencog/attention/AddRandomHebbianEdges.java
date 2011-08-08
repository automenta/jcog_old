/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.attention;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;
import org.apache.commons.collections15.iterators.FilterIterator;

/**
 *
 * @author seh
 */
public class AddRandomHebbianEdges extends MindAgent {
    private static final Logger logger = Logger.getLogger(AddRandomHebbianEdges.class.getSimpleName());
    
    private int minEdges = 16;
    private int addsPerCycle, maxEdges;
    
    Set<Atom> edgesAdded = new HashSet();
    
    private boolean avoidAddedEdges = true;
    
    private boolean removeUselessHebbianEdges = true;
    private int removalsPerCycle;
    
    public AddRandomHebbianEdges(double period, int edgeAttemptsPerCycle, int edgeRemovalsPerCycle, int minEdges, int maxEdges) {
        super(period);
        setEdgeAttemptsPerCycle(edgeAttemptsPerCycle, edgeRemovalsPerCycle);
        setMinEdges(minEdges);
        setMaxEdges(maxEdges);
    }

    public void setMinEdges(int minEdges) {
        this.minEdges = minEdges;
    }        

    public void setMaxEdges(int maxEdges) {
        this.maxEdges = maxEdges;
    }
    
    @Override
    protected void run(final OCMind mind) {
        Predicate<? super Atom> excludeHebbian = new Predicate<Atom>() {

            @Override
            public boolean evaluate(final Atom t) {
                return mind.getType(t)!=AtomType.symmetricHebbianLink;
            }            
        };
        List<Atom> la = IteratorUtils.toList(new FilterIterator<Atom>(mind.iterateAtoms(), excludeHebbian));
        
        int numAdded = 0, numRemoved = 0;
        
        if (la.size() < 2)
            return;
        
        for (int i = 0; i < addsPerCycle; i++) {
            if (edgesAdded.size() >= maxEdges)
                return;
            
            int a = RandomNumber.getInt(0, la.size()-1);
            int b = a;
            while (b == a) {
                b = RandomNumber.getInt(0, la.size()-1);
            }
            
            
            
            Atom aa = la.get(a);
            Atom bb = la.get(b);
            if (mind.getEdge(AtomType.symmetricHebbianLink, aa, bb) == null) {
                if (mind.getEdge(AtomType.symmetricHebbianLink, bb, aa) == null) {
                    //logger.info("adding random hebbian between: " + aa + " " + bb + " #" + edgesAdded.size());
                    
                    Atom e = mind.addEdge(AtomType.symmetricHebbianLink, aa, bb);
                    edgesAdded.add(e);
                    numAdded++;
                }                
            }
            
        }
        
        if (removeUselessHebbianEdges) {
            List<Atom> removed = new LinkedList();
            
            List<Atom> lEdges = new LinkedList(edgesAdded);
            Collections.sort(lEdges, new Comparator<Atom>() {

                @Override
                public int compare(Atom o1, Atom o2) {
                    double t1 = mind.getTruth(o1).getMean();
                    double t2 = mind.getTruth(o2).getMean();
                    if (t1 > t2) return -1;
                    else if (t1 == t2) return 0;
                    return 1;
                }
                
            });
            
            if (lEdges.size() > minEdges) {
                for (int i = 0; i < Math.min(removalsPerCycle, lEdges.size() - minEdges); i++) {
                    final Atom e = lEdges.get(i);
                    removeEdge(e);
                    removed.add(e);
                }
            }
            
            numRemoved += removed.size();
            edgesAdded.removeAll(removed);
        }
        
        //System.out.println(this + ": "+ numAdded + " / " + numRemoved);
    }

    private void setEdgeAttemptsPerCycle(int additions, int removals) {
        this.addsPerCycle = additions;
        this.removalsPerCycle = removals;
    }
    
}
