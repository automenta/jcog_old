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
import edu.uci.ics.jung.graph.util.Pair;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import javax.media.opengl.GL2;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.MemoryAtomSpace;
import jcog.opencog.OCMind;
import jcog.opencog.atom.TruthValue;
import jcog.spacegraph.*;
import jcog.spacegraph.math.linalg.Vec3f;
import jcog.spacegraph.shape.Curve;
import jcog.spacegraph.shape.Drawable;
import jcog.spacegraph.shape.Rect;
import jcog.spacegraph.shape.TextRect;
import jcog.spacegraph.swing.SwingWindow;
import jcog.spacegraph.ui.PointerLayer;
import org.apache.commons.collections15.Factory;

/**
 *
 * @author seh
 */
public class GraphView1 extends AbstractSurfaceDemo implements Drawable {
    private final MemoryAtomSpace graph;

    private HashMap<Atom, Rect> atomRect = new HashMap();
    private HashMap<Pair<Atom>, Curve> edgeCurve = new HashMap();
    
    private final OCMind mind;
    private Layout<Atom,Atom> layout;
    
    final static TextRenderer textRenderer = TextRect.newTextRenderer(new Font("Arial", Font.PLAIN, 16));    

    @Override
    public String getName() {
        return "GraphView1";
    }

    @Override
    public String getDescription() {
        return "Graph View";
    }

    public GraphView1(OCMind mind) {
        super();
        
        this.mind = mind;
        this.graph = mind.atomspace;
        
        
        //add(new GridRect(6, 6));

        add(this);
        
        add(new PointerLayer(this));

        updateGraph();
    }

    protected void addVertex(final Atom v) {
        Rect r = atomRect.get(v);
        if (r == null) {
            String name = mind.getName(v);
            r = new TextRect(textRenderer, name);
            //r.setnewInitialPosition(), newInitialScale());
            atomRect.put(v, r);
        }
    }
    
    private void updateRect(Rect r, double sti) {
        float sx = 0.1f + (float)sti * 0.2f;
        sx = Math.min(sx, 0.5f);
        r.setScale(sx, sx, 1.0f);
    }
    
    protected Vec3f newInitialPosition() {
        final float z = 0.5f;
        float r = 4.0f;
        return new Vec3f(RandomNumber.getFloat(-r, r), RandomNumber.getFloat(-r, r), z);
    }
    
    protected Vec3f newInitialScale() {
        return new Vec3f(0.1f, 0.1f, 1.0f);
    }
    
    private void addEdge(Atom s, Atom t) {
        Pair<Atom> p = new Pair(s, t);
        Curve c = edgeCurve.get(p);
        if (c==null) {
            c = new Curve(atomRect.get(s), atomRect.get(t), 2, 1f);
            edgeCurve.put(p, c);
        }
    }

    private void updateCurve(Atom s, Atom t, Curve c) {
        TruthValue tv = mind.getTruth(s);
        float w = 1f + (float)tv.getMean() * 4f;
        c.setLineWidth(w);       
        c.setColor(0.5f + 0.5f * (float)tv.getMean(), 0, 0);
    }
    
    protected void addEdge(Atom e) {
        addVertex(e);
        
        final List<Atom> iv = new ArrayList(mind.getIncidentVertices(e));
        if (iv.size() == 1) {
            addEdge(e, iv.get(0));
        }
        else {
            for (Atom i : iv) {
                addVertex(i);
                addEdge(e, i);
            }
        }
        
    }
    
    protected void updateGraph() {
        for (Atom v : graph.getVertices()) {
            addVertex(v);
        }
        for (Atom e : graph.getEdges()) {
            addEdge(e);
        }
        
        Graph<Atom, Atom> fg = FoldingTransformer.foldHypergraphEdges(mind.atomspace.graph, new Factory<Graph<Atom, Atom>>() {

                              @Override
                              public Graph<Atom, Atom> create() {
                                  return new DirectedSparseMultigraph();
                              }
                              
                          }, new Factory<Atom>() {

                              @Override
                              public Atom create() {
                                  return new Atom();
                              }
                                      
                          });

//             Graph<Atom, Atom> fg = FoldingTransformer.foldHypergraphVertices(mind.atomspace.graph, new Factory<Graph<Atom, Atom>>() {
//
//                              @Override
//                              public Graph<Atom, Atom> create() {
//                                  return new DirectedSparseMultigraph();
//                              }
//                              
//                          }, new Factory<Atom>() {
//
//                              @Override
//                              public Atom create() {
//                                  return new Atom();
//                              }
//                                      
//                          });

        layout = new SpringLayout(fg);
        //layout = new ISOMLayout(fg);
        //layout = new KKLayout(fg);
        layout.setSize(new Dimension(500, 500));
        layout.initialize();
        
        
    }
    
    protected void layoutGraph() {
        for (Atom v : atomRect.keySet()) {
            updateRect(atomRect.get(v), mind.getNormalizedSTI(v));
        }
        for (Pair<Atom> e : edgeCurve.keySet()) {
            updateCurve(e.getFirst(), e.getSecond(), edgeCurve.get(e));
            
        }
        

        ((IterativeContext)layout).step();
        
        for (Entry<Atom,Rect> i : atomRect.entrySet()) {
            Point2D p = layout.transform(i.getKey());            
            float x = -5.0f + (float)p.getX() / 100.0f;
            float y = -5.0f + (float)p.getY() / 100.0f;
            i.getValue().setCenter(x, y);
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
        

    public static void newGraphView(OCMind mind) {
        new SwingWindow(AbstractSurfaceDemo.newPanel(new GraphView1(mind)), 800, 800, true);
    }



}
