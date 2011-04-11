/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.hopfield;

import com.sun.opengl.util.awt.TextRenderer;
import com.syncleus.dann.graph.AbstractDirectedEdge;
import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import com.syncleus.dann.graph.drawing.hyperassociativemap.HyperassociativeMap;
import com.syncleus.dann.math.Vector;
import edu.uci.ics.jung.graph.Hypergraph;
import java.awt.Color;
import java.awt.Font;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.media.opengl.GL2;
import javax.swing.JPanel;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.MemoryAtomSpace;
import jcog.opencog.OCMind;
import jcog.opencog.atom.TruthValue;
import jcog.spacegraph.*;
import jcog.spacegraph.math.linalg.Vec3f;
import jcog.spacegraph.shape.Drawable;
import jcog.spacegraph.shape.TextRect;
import jcog.spacegraph.shape.TrapezoidLine;
import jcog.spacegraph.swing.SwingWindow;
import jcog.spacegraph.ui.PointerLayer;

/**
 *
 * @author seh
 */
public class GraphView2 extends AbstractSurfaceDemo implements Drawable {
    private final MemoryAtomSpace graph;

    private HashMap<Atom, TextRect> atomRect = new HashMap();
    private HashMap<FoldedEdge, TrapezoidLine> edgeCurve = new HashMap();
    final int alignCycles = 1;
    
    private final OCMind mind;
    private HyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, FoldedEdge>, Atom> ham;
    
    final static TextRenderer textRenderer = TextRect.newTextRenderer(new Font("Arial", Font.PLAIN, 32));

    
    
    private MutableDirectedAdjacencyGraph<Atom, FoldedEdge> digraph;

    @Override
    public String getName() {
        return "GraphView1";
    }

    @Override
    public String getDescription() {
        return "Graph View";
    }

    public GraphView2(OCMind mind) {
        super();
        
        this.mind = mind;
        this.graph = mind.atomspace;
        
        //add(new GridRect(6, 6));

        add(this);
        
        add(new PointerLayer(this));

        updateGraph();
    }

    protected void addVertex(final Atom v) {
        TextRect r = atomRect.get(v);
        if (r == null) {
            String name = mind.getName(v);
            if (name == null)
                name = "";
            
            r = new TextRect(textRenderer, name);
            //r.setnewInitialPosition(), newInitialScale());
            atomRect.put(v, r);
        }
    }
    
    private void updateRect(TextRect r, double sti) {
        float sx = 0.1f + (float)sti * 0.2f;
        sx = Math.min(sx, 0.5f);
        r.setScale(sx, sx, 1.0f);
        r.setFilled(true);
    }
    
    protected Vec3f newInitialPosition() {
        final float z = 0.5f;
        float r = 4.0f;
        return new Vec3f(RandomNumber.getFloat(-r, r), RandomNumber.getFloat(-r, r), z);
    }
    
    protected Vec3f newInitialScale() {
        return new Vec3f(0.1f, 0.1f, 1.0f);
    }
    
    private void addEdge(final FoldedEdge e) {
        final Atom s = e.getSourceNode();
        final Atom t = e.getDestinationNode();
        TrapezoidLine c = edgeCurve.get(e);
        if (c==null) {
            c = new TrapezoidLine(atomRect.get(s), atomRect.get(t), 0.1f, 0.05f);
            edgeCurve.put(e, c);
        }
    }

    protected static class FoldedEdge extends AbstractDirectedEdge<Atom> {
        private final String label;
        private final Atom parentEdge;

        public FoldedEdge(Atom src, Atom dest, Atom parentEdge, String label) {
            super(src, dest);
            this.label = label;
            this.parentEdge = parentEdge;
        }

        @Override
        public String toString() {
            return label;
        }        
        
    }

    private void updateCurve(FoldedEdge e) {
        final TrapezoidLine c = edgeCurve.get(e);
        
        final Atom truthSource = ((FoldedEdge)e).parentEdge;
        
        TruthValue tv = mind.getTruth(truthSource);
        final float w = 1f + (float)tv.getMean() * 4f;
        c.setSourceWidth(w/20.0f);       
        c.setEndWidth(w/60.0f);       
        
        final float v = 0.5f + 0.5f * (float)tv.getMean();
        
        final float hue = ((float)(Math.abs(mind.getType(truthSource).getName().hashCode()))%100)/100.0f;
                
        final Color h = Color.getHSBColor(hue, 0.8f, 1.0f);
        float[] hRGB = h.getColorComponents(null);
        c.setColor(v*hRGB[0], v*hRGB[1], v*hRGB[2]);
    }
    
//    protected void addEdge(Atom e) {
//        addVertex(e);
//        
//        final List<Atom> iv = new ArrayList(mind.getIncidentVertices(e));
//        if (iv.size() == 1) {
//            addEdge(e, iv.get(0));
//        }
//        else {
//            for (Atom i : iv) {
//                addVertex(i);
//                addEdge(e, i);
//            }
//        }
//        
//    }

    
    /**
     * Creates a <code>Graph</code> which is an edge-folded version of <code>h</code>, where
     * hyperedges are replaced by k-cliques in the output graph.
     * 
     * <p>The vertices of the new graph are the same objects as the vertices of 
     * <code>h</code>, and <code>a</code> 
     * is connected to <code>b</code> in the new graph if the corresponding vertices
     * in <code>h</code> are connected by a hyperedge.  Thus, each hyperedge with 
     * <i>k</i> vertices in <code>h</code> induces a <i>k</i>-clique in the new graph.</p>
     * 
     * <p>The edges of the new graph are generated by the specified edge factory.</p>
     * 
     * @param <V> vertex type
     * @param <E> input edge type
     * @param h hypergraph to be folded
     * @param graph_factory factory used to generate the output graph
     * @param edge_factory factory used to create the new edges 
     * @return a copy of the input graph where hyperedges are replaced by cliques
     */
    public MutableDirectedAdjacencyGraph<Atom, FoldedEdge> foldHypergraphEdges(final MutableDirectedAdjacencyGraph<Atom, FoldedEdge> target, final Hypergraph<Atom,Atom> h, boolean linkEdgeToMembers)
    {
        for (Atom v : h.getVertices()) {
            target.add(v);            
        }
        
        for (Atom e : h.getEdges())
        {
            target.add(e);            
            
            ArrayList<Atom> incident = new ArrayList(h.getIncidentVertices(e));
            
            if (linkEdgeToMembers) {
                for (int i = 0; i < incident.size(); i++) {                
                   target.add(new FoldedEdge(e, incident.get(i), e, "(") );
                   if (i > 0)
                        target.add(new FoldedEdge(incident.get(i-1), incident.get(i), e, Integer.toString(i)) );
                }
            }
            else {
                final String typeString = mind.getType(e).toString();
                
                //Just link the edge to the first element
                for (int i = 0; i < incident.size(); i++) {                

                   if (i > 0)
                        target.add(new FoldedEdge(incident.get(i-1), incident.get(i), e, Integer.toString(i)));
                   else 
                        target.add(new FoldedEdge(e, incident.get(i), e, "(" + typeString));
                }
                
            }
        
        }
        return target;
    }
    
    protected void updateGraph() {
        for (Atom v : graph.getVertices()) {
            addVertex(v);
        }
        for (Atom v : graph.getEdges()) {
            addVertex(v);
        }
        
        digraph = foldHypergraphEdges(new MutableDirectedAdjacencyGraph<Atom, FoldedEdge>(),
                mind.atomspace.graph, false);

        
        for (FoldedEdge e : digraph.getEdges()) {
            addEdge(e);
        }
        
        int numDimensions = 2;
        ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
        ham = new HyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, FoldedEdge>, Atom>(digraph, numDimensions, executor);
        //ham.setEquilibriumDistance(1.0);
  
        
        
    }
    
    protected void layoutGraph() {
        for (Atom v : atomRect.keySet()) {
            updateRect(atomRect.get(v), mind.getNormalizedSTI(v));
        }
        for (FoldedEdge e : digraph.getEdges()) {
            updateCurve(e);            
        }
        
        
        for (int i = 0; i < alignCycles; i++)
            ham.align();
        
        
        final float s = 0.2f;
        for (Entry<Atom,TextRect> i : atomRect.entrySet()) {
            final Vector v = ham.getCoordinates().get(i.getKey());
            if (v.getDimensions() == 2) {
                float x = (float)v.getCoordinate(1)*s;
                float y = (float)v.getCoordinate(2)*s;
                i.getValue().setCenter(x, y);
            }
            else if (v.getDimensions() == 3) {
                float x = (float)v.getCoordinate(1)*s;
                float y = (float)v.getCoordinate(2)*s;
                float z = (float)v.getCoordinate(3)*s;
                i.getValue().setCenter(x, y, z);
            }
        }
    }
    
    protected void drawAtoms(GL2 gl) {
        for (Drawable d : atomRect.values()) {
            d.draw(gl);
        }
    }
    protected void drawEdges(GL2 gl) {
        for (Drawable d : edgeCurve.values()) {
            d.draw(gl);
        }        
    }
    
    @Override
    public void draw(GL2 gl) {
        layoutGraph();
        drawAtoms(gl);
        drawEdges(gl);
    }
        
    public static JPanel newGraphPanel(OCMind mind) {
        return AbstractSurfaceDemo.newPanel(new GraphView2(mind));
    }

    public static void newGraphWindow(OCMind mind) {
        new SwingWindow(AbstractSurfaceDemo.newPanel(new GraphView2(mind)), 800, 800, true);
    }



}
