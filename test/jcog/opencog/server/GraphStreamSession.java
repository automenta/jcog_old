/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.server;

import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import jcog.opencog.Atom;
import jcog.opencog.OCMind;
import jcog.opencog.swing.graph.HyperedgeSegment;
import org.apache.commons.collections15.IteratorUtils;

/**
 * see: http://wiki.gephi.org/index.php/Specification_-_GSoC_Graph_Streaming_API
 *
 */
/**
 *
 * @author seh
 */
public class GraphStreamSession implements Runnable {
    public final PipedOutputRepresentation representation;
    private PipedOutputStream output;
    private final OCMind mind;
    int maxAtoms = 4096;
    private final Map<Atom, Object> vertexShape = new HashMap();
    private final Map<HyperedgeSegment, Object> edgeShape = new HashMap();
    private MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment> digraph;
    private List<Atom> verticesToAdd = new LinkedList();
    private List<Atom> verticesToRemove = new LinkedList();
    private List<HyperedgeSegment> edgesToAdd = new LinkedList();
    private List<HyperedgeSegment> edgesToRemove = new LinkedList();

    public GraphStreamSession(OCMind mind) {
        final PipedInputStream pi = new PipedInputStream();
        this.mind = mind;
        this.representation = new PipedOutputRepresentation(pi);
        try {
            output = new PipedOutputStream(pi);
            new Thread(this).start();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    public Object addVertex(final Atom v) {
        Object r = vertexShape.get(v);
        if (r == null) {
            String name = mind.getName(v);
            if (name == null) {
                name = "";
            }
            vertexShape.put(v, new Object());
            verticesToAdd.add(v);
        }
        return r;
    }

    public Object addEdge(final HyperedgeSegment e) {
        final Atom s = e.getSourceNode();
        final Atom t = e.getDestinationNode();
        Object c = edgeShape.get(e);
        if (c == null) {
            c = new Object();
            edgeShape.put(e, c);
            edgesToAdd.add(e);
        }
        return c;
    }

    public void removeVertex(final Atom v) {
        vertexShape.remove(v);
    }

    public void removeEdge(final HyperedgeSegment e) {
        edgeShape.remove(e);
    }

    public void write(String s) throws IOException {
        output.write((s + "\r").getBytes(Charset.forName("UTF-8")));
    }

    public Collection<HyperedgeSegment> getVisibleEdges() {
        return Collections.unmodifiableCollection(edgeShape.keySet());
    }

    protected void update() {
        int remained = 0;
        int removed = 0;
        int added = 0;
        List<Atom> arank = IteratorUtils.toList(mind.iterateAtomsByDecreasingSTI());
        final int n = Math.min(arank.size(), maxAtoms);
        final List<Atom> highest = arank.subList(0, n);
        final Set<Atom> hm = new HashSet(highest); //use set for faster contains()
        final List<Atom> verticesToRemove = new LinkedList();
        final List<Atom> verticesToAdd = new LinkedList();
        for (final Atom v : vertexShape.keySet()) {
            if (!hm.contains(v)) {
                verticesToRemove.add(v);
                removed++;
            } else {
                remained++;
            }
        }
        for (final Atom v : hm) {
            if (!vertexShape.containsKey(v)) {
                verticesToAdd.add(v);
                added++;
            }
        }
        for (final Atom a : verticesToRemove) {
            removeVertex(a);
        }
        for (final Atom a : verticesToAdd) {
            addVertex(a);
        }
        //----------------
        digraph = mind.foldHypergraphEdges(vertexShape.keySet(), new MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment>(), true);
        Collection<HyperedgeSegment> diEdges = digraph.getEdges();
        final List<HyperedgeSegment> edgesToRemove = new LinkedList();
        final List<HyperedgeSegment> edgesToAdd = new LinkedList();
        for (final HyperedgeSegment v : edgeShape.keySet()) {
            if (!digraph.getEdges().contains(v)) {
                edgesToRemove.add(v);
                removed++;
            } else {
                remained++;
            }
        }
        for (final HyperedgeSegment v : diEdges) {
            if (!getVisibleEdges().contains(v)) {
                edgesToAdd.add(v);
                added++;
            }
        }
        for (final HyperedgeSegment a : edgesToRemove) {
            removeEdge(a);
        }
        for (final HyperedgeSegment a : edgesToAdd) {
            addEdge(a);
        }
    }

    @Override
    public void run() {
        int n = 0;
        while (true) {
            try {
                update();
                if (!verticesToAdd.isEmpty()) {
                    for (Atom a : verticesToAdd) {
                        String id = a.uuid.toString();
                        String name = mind.getName(a);
                        write("{\"an\":{\"" + id + "\":{\"label\":\"" + name + "\"}}}");
                    }
                    verticesToAdd.clear();
                }
                if (!edgesToAdd.isEmpty()) {
                    for (HyperedgeSegment a : edgesToAdd) {
                        Atom aa = a.getSourceNode();
                        Atom bb = a.getDestinationNode();
                        String id = a.parentEdge.uuid.toString();
                        String aaI = aa.uuid.toString();
                        String bbI = bb.uuid.toString();
                        write("{\"ae\":{\"" + id + "\":{\"source\":\"" + aaI + "\",\"target\":\"" + bbI + "\",\"directed\":true,\"weight\":2}}}");
                    }
                    edgesToAdd.clear();
                }
                /*
                {"an":{"A":{"label":"Streaming Node A","size":2}}} // add node A
                {"an":{"B":{"label":"Streaming Node B","size":1}}} // add node B
                {"an":{"C":{"label":"Streaming Node C","size":1}}} // add node C
                {"ae":{"AB":{"source":"A","target":"B","directed":false,"weight":2}}} // add edge A->B
                {"ae":{"BC":{"source":"B","target":"C","directed":false,"weight":1}}} // add edge B->C
                {"ae":{"CA":{"source":"C","target":"A","directed":false,"weight":2}}} // add edge C->A
                {"cn":{"C":{"size":2}}}  // changes the size attribute to 2
                {"cn":{"B":{"label":null}}}  // removes the label attribute
                {"ce":{"AB":{"label":"From A to B"}}} // add the label attribute
                {"de":{"BC":{}}} // delete edge BC
                {"de":{"CA":{}}} // delete edge CA
                {"dn":{"C":{}}}  // delete node C
                 */
                n++;
            } catch (IOException ex) {
                break;
            }
            try {
                Thread.sleep(1500);
            } catch (InterruptedException ex) {
                ex.printStackTrace();
            }
        }
    }
    
}
