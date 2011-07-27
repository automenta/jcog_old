/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.hopfield;

import com.sun.opengl.util.awt.TextRenderer;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.algorithms.layout.SpringLayout;
import edu.uci.ics.jung.algorithms.transformation.FoldingTransformer;
import edu.uci.ics.jung.algorithms.util.IterativeContext;
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.Hypergraph;
import edu.uci.ics.jung.graph.util.Pair;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map.Entry;
import javax.media.opengl.GL2;
import javax.swing.JPanel;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.atom.MemoryAtomSpace;
import jcog.opencog.OCMind;
import jcog.opencog.atom.TruthValue;
import jcog.spacegraph.*;
import jcog.spacegraph.math.linalg.Vec3f;
import jcog.spacegraph.shape.Drawable;
import jcog.spacegraph.shape.TextRect;
import jcog.spacegraph.shape.TrapezoidLine;
import jcog.spacegraph.swing.SwingWindow;
import jcog.spacegraph.ui.PointerLayer;
import org.apache.commons.collections15.Factory;

/**
 *
 * @author seh
 */
@Deprecated public class GraphView1 /*extends AbstractSurfaceDemo implements Drawable*/ {
//    private final MemoryAtomSpace graph;
//
//    private HashMap<Atom, TextRect> atomRect = new HashMap();
//    private HashMap<Atom, TrapezoidLine> edgeCurve = new HashMap();
//    
//    private final OCMind mind;
//    private Layout<Atom,Atom> layout;
//    
//    final static TextRenderer textRenderer = TextRect.newTextRenderer(new Font("Arial", Font.PLAIN, 32));
//    private Graph<Atom, Atom> digraph;
//
//    @Override
//    public String getName() {
//        return "GraphView1";
//    }
//
//    @Override
//    public String getDescription() {
//        return "Graph View";
//    }
//
//    public GraphView1(OCMind mind) {
//        super();
//        
//        this.mind = mind;
//        this.graph = mind.atomspace;
//        
//        
//        //add(new GridRect(6, 6));
//
//        add(this);
//        
//        add(new PointerLayer(this));
//
//        updateGraph();
//    }
//
//    protected void addVertex(final Atom v) {
//        TextRect r = atomRect.get(v);
//        if (r == null) {
//            String name = mind.getName(v);
//            if (name == null)
//                name = "";
//            
//            r = new TextRect(textRenderer, name);
//            //r.setnewInitialPosition(), newInitialScale());
//            atomRect.put(v, r);
//        }
//    }
//    
//    private void updateRect(TextRect r, double sti) {
//        float sx = 0.1f + (float)sti * 0.2f;
//        sx = Math.min(sx, 0.5f);
//        r.setScale(sx, sx, 1.0f);
//        r.setFilled(true);
//    }
//    
//    protected Vec3f newInitialPosition() {
//        final float z = 0.5f;
//        float r = 4.0f;
//        return new Vec3f(RandomNumber.getFloat(-r, r), RandomNumber.getFloat(-r, r), z);
//    }
//    
//    protected Vec3f newInitialScale() {
//        return new Vec3f(0.1f, 0.1f, 1.0f);
//    }
//    
//    private void addEdge(Atom e, Atom s, Atom t) {
//        //Pair<Atom> p = new Pair(s, t);
//        TrapezoidLine c = edgeCurve.get(e);
//        if (c==null) {
//            c = new TrapezoidLine(atomRect.get(s), atomRect.get(t), 0.1f, 0.05f);
//            edgeCurve.put(e, c);
//        }
//    }
//
//    protected static class LabeledAtom extends Atom {
//        private final String label;
//        private final Atom parentEdge;
//
//        public LabeledAtom(Atom parentEdge, String label) {
//            super();
//            this.label = label;
//            this.parentEdge = parentEdge;
//        }
//
//        @Override
//        public String toString() {
//            return label;
//        }        
//        
//    }
//
//    private void updateCurve(Atom e) {
//        final TrapezoidLine c = edgeCurve.get(e);
//        
//        final Atom truthSource;
//        if (e instanceof LabeledAtom)
//            truthSource = ((LabeledAtom)e).parentEdge;
//        else 
//            truthSource = e;
//        
//        TruthValue tv = mind.getTruth(truthSource);
//        final float w = 1f + (float)tv.getMean() * 4f;
//        c.setSourceWidth(w/20.0f);       
//        c.setEndWidth(w/60.0f);       
//        
//        final float v = 0.5f + 0.5f * (float)tv.getMean();
//        
//        final float hue = ((float)(Math.abs(mind.getType(truthSource).getName().hashCode()))%100)/100.0f;
//                
//        final Color h = Color.getHSBColor(hue, 0.7f, 0.8f);
//        float[] hRGB = h.getColorComponents(null);
//        c.setColor(v*hRGB[0], v*hRGB[1], v*hRGB[2]);
//    }
//    
////    protected void addEdge(Atom e) {
////        addVertex(e);
////        
////        final List<Atom> iv = new ArrayList(mind.getIncidentVertices(e));
////        if (iv.size() == 1) {
////            addEdge(e, iv.get(0));
////        }
////        else {
////            for (Atom i : iv) {
////                addVertex(i);
////                addEdge(e, i);
////            }
////        }
////        
////    }
//
//    
//    /**
//     * Creates a <code>Graph</code> which is an edge-folded version of <code>h</code>, where
//     * hyperedges are replaced by k-cliques in the output graph.
//     * 
//     * <p>The vertices of the new graph are the same objects as the vertices of 
//     * <code>h</code>, and <code>a</code> 
//     * is connected to <code>b</code> in the new graph if the corresponding vertices
//     * in <code>h</code> are connected by a hyperedge.  Thus, each hyperedge with 
//     * <i>k</i> vertices in <code>h</code> induces a <i>k</i>-clique in the new graph.</p>
//     * 
//     * <p>The edges of the new graph are generated by the specified edge factory.</p>
//     * 
//     * @param <V> vertex type
//     * @param <E> input edge type
//     * @param h hypergraph to be folded
//     * @param graph_factory factory used to generate the output graph
//     * @param edge_factory factory used to create the new edges 
//     * @return a copy of the input graph where hyperedges are replaced by cliques
//     */
//    public Graph<Atom,Atom> foldHypergraphEdges(final Graph<Atom,Atom> target, final Hypergraph<Atom,Atom> h, boolean linkEdgeToMembers)
//    {
//        for (Atom v : h.getVertices()) {
//            target.addVertex(v);            
//        }
//        
//        for (Atom e : h.getEdges())
//        {
//            target.addVertex(e);            
//            
//            ArrayList<Atom> incident = new ArrayList(h.getIncidentVertices(e));
//            
//            if (linkEdgeToMembers) {
//                for (int i = 0; i < incident.size(); i++) {                
//                   target.addEdge(new LabeledAtom(e, "("), e, incident.get(i));
//                   if (i > 0)
//                        target.addEdge(new LabeledAtom(e, Integer.toString(i)), incident.get(i-1), incident.get(i));
//                }
//            }
//            else {
//                //Just link the edge to the first element
//                for (int i = 0; i < incident.size(); i++) {                
//                   if (i > 0)
//                        target.addEdge(new LabeledAtom(e, Integer.toString(i)), incident.get(i-1), incident.get(i));
//                   else 
//                        target.addEdge(new LabeledAtom(e, "(" + mind.getType(e)), e, incident.get(i));
//                }
//                
//            }
//        
//        }
//        return target;
//    }
//    
//    protected void updateGraph() {
//        for (Atom v : graph.getVertices()) {
//            addVertex(v);
//        }
//        for (Atom v : graph.getEdges()) {
//            addVertex(v);
//        }
//        
//        digraph = foldHypergraphEdges(new DirectedSparseMultigraph(), mind.atomspace.graph, false);
//
////             Graph<Atom, Atom> digraph = FoldingTransformer.foldHypergraphVertices(mind.atomspace.graph, new Factory<Graph<Atom, Atom>>() {
////
////                              @Override
////                              public Graph<Atom, Atom> create() {
////                                  return new DirectedSparseMultigraph();
////                              }
////                              
////                          }, new Factory<Atom>() {
////
////                              @Override
////                              public Atom create() {
////                                  return new Atom();
////                              }
////                                      
////                          });
//        
//        for (Atom e : digraph.getEdges()) {
//            addEdge(e, digraph.getSource(e), digraph.getDest(e));
//        }
//
//        layout = new SpringLayout(digraph);
//        //layout = new ISOMLayout(digraph);
//        //layout = new KKLayout(digraph);
//        layout.setSize(new Dimension(1000, 1000));
//        layout.initialize();
//        
//        
//    }
//    
//    protected void layoutGraph() {
//        for (Atom v : atomRect.keySet()) {
//            updateRect(atomRect.get(v), mind.getNormalizedSTI(v));
//        }
//        for (Atom e : digraph.getEdges()) {
//            updateCurve(e);            
//        }
//        
//
//        ((IterativeContext)layout).step();
//        
//        for (Entry<Atom,TextRect> i : atomRect.entrySet()) {
//            Point2D p = layout.transform(i.getKey());            
//            float x = -5.0f + (float)p.getX() / 100.0f;
//            float y = -5.0f + (float)p.getY() / 100.0f;
//            i.getValue().setCenter(x, y);
//        }
//    }
//    
//    protected void drawAtoms(GL2 gl) {
//        for (Drawable d : atomRect.values()) {
//            d.draw(gl);
//        }
//    }
//    protected void drawEdges(GL2 gl) {
//        for (Drawable d : edgeCurve.values()) {
//            d.draw(gl);
//        }        
//    }
//    
//    @Override
//    public void draw(GL2 gl) {
//        layoutGraph();
//        drawAtoms(gl);
//        drawEdges(gl);
//    }
//        
//    public static JPanel newGraphPanel(OCMind mind) {
//        return AbstractSurfaceDemo.newPanel(new GraphView1(mind));
//    }
//
//    public static void newGraphWindow(OCMind mind) {
//        new SwingWindow(AbstractSurfaceDemo.newPanel(new GraphView1(mind)), 800, 800, true);
//    }
//
//

}
