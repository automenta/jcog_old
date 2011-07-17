/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing;

import com.sun.opengl.util.awt.TextRenderer;
import com.syncleus.dann.graph.AbstractDirectedEdge;
import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import com.syncleus.dann.math.Vector;
import edu.uci.ics.jung.graph.Hypergraph;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.media.opengl.GL2;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.OCMind;
import jcog.opencog.atom.TruthValue;
import jcog.spacegraph.gl.Surface;
import jcog.spacegraph.math.linalg.Vec2f;
import jcog.spacegraph.math.linalg.Vec3f;
import jcog.spacegraph.math.linalg.Vec4f;
import jcog.spacegraph.shape.Drawable;
import jcog.spacegraph.shape.Spatial;
import jcog.spacegraph.shape.TextRect;
import jcog.spacegraph.shape.TrapezoidLine;
import jcog.spacegraph.swing.SwingWindow;
import jcog.spacegraph.ui.PointerLayer;

/**
 *
 * @author seh
 */
public class GraphView extends Surface implements Drawable {

    final public static ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
    
    private HashMap<Atom, TextRect> atomRect = new HashMap();
    private HashMap<FoldedEdge, TrapezoidLine> edgeCurve = new HashMap();
    
    private final OCMind mind;
    
    final static TextRenderer textRenderer = TextRect.newTextRenderer(new Font("Arial", Font.PLAIN, 32));    
    
    private MutableDirectedAdjacencyGraph<Atom, FoldedEdge> digraph;
    
    private final GraphViewModel param;
    private long currentTime = 0;
    private long lastTime = 0;
    private short minSTI, maxSTI;
    
    public static interface GraphViewModel {

        public Vec4f getVertexColor(Atom vertex);

        public Vec2f getVertexScale(Atom vertex, short maxSTI, short minSTI);
        
        public float[] getCurveProfile(Atom edge);
        public Vec3f getCurveColor(Atom edge);

        public float getVertexEquilibriumDistance(Atom n);

        public float getMeanEquilibriumDistance();

        public double getLayoutUpdatePeriod();

        public float getInterpolationMomentum();

        public int getMaxAtoms();
        
        //does the graph need updated?
        public boolean getUpdateGraph();
        
    }
    
    public static class SeHGraphViewModel1 implements GraphViewModel {
        private final OCMind mind;

        private final JPanel control = new JPanel();
        private boolean updateGraph = false;
        
        public SeHGraphViewModel1(final OCMind mind) {
            super();
            this.mind = mind;
            
            
            control.setLayout(new BoxLayout(control, BoxLayout.PAGE_AXIS));
            {
                JButton updateButton = new JButton("Update");
                updateButton.addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        updateGraph = true;
                    }                    
                });
                control.add(updateButton);
                
                addSlider("MeanEquilibriumDistance", 2.0, 0.1, 6.0);
                addSlider("VertexScale", 0.2, 0.1, 1.0);
                addSlider("EdgeWidthScale", 0.05, 0.001, 0.2);
            }
            control.add(Box.createVerticalBox());
            
            new SwingWindow(new JScrollPane(control), 200, 500);
            
        }
        
        final int integerScale = 200;

        public int toSlider(double v) {
            return (int)(v * integerScale);
        }
        public double fromSlider(int x) {
            return (double)x / ((double)integerScale);
        }
        
        Map<String,JSlider> sliders = new HashMap();
        
        public JSlider addSlider(String methodSuffix, double initialValue, double min, double max) {
            
            JSlider js = new JSlider(JSlider.HORIZONTAL);
            js.setValue(toSlider(initialValue));
            js.setMinimum(toSlider(min));
            js.setMaximum(toSlider(max));
            
            sliders.put(methodSuffix, js);
            
            control.add(new JLabel(methodSuffix));
            control.add(js);
            
            return js;
        }

        double getSliderValue(final String method) {
            return fromSlider(sliders.get(method).getValue() );
        }
        
        @Override
        public Vec4f getVertexColor(Atom v) {
            final double sti = mind.getNormalizedSTI(v);
            return new Vec4f(0.5f+((float)sti/100.0f), 0.5f, 0.5f, 1.0f);
        }
        
        @Override
        public Vec2f getVertexScale(Atom v, short maxSTI, short minSTI) {
            final float vertexScale = (float)getSliderValue("VertexScale");
            final double sti = mind.getNormalizedSTI(v, maxSTI, minSTI) + 0.5;
            float sx = 0.1f + (float)(sti*sti) * vertexScale;
            sx = Math.min(sx, 0.5f);
            return new Vec2f(sx, sx);
        }

        @Override
        public float[] getCurveProfile(Atom edge) {
            final TruthValue tv = mind.getTruth(edge);
            final float w = 1f + (float)tv.getMean() * 4f;
            final float edgeWidthScale = (float)getSliderValue("EdgeWidthScale");
            final float edgeRatio = 3.0f;
            return new float[] { w * edgeWidthScale, w * (edgeWidthScale/edgeRatio) };
        }


        @Override
        public Vec3f getCurveColor(Atom edge) {
            final float v = 0.7f + 0.3f * (float)mind.getTruth(edge).getMean();
            
            final float hue = ((Math.abs(mind.getType(edge).getName().hashCode()+10005+(int)v))%100)/100.0f;

            final Color h = Color.getHSBColor(hue, 0.85f, v);
            float[] hRGB = h.getColorComponents(null);
            return new Vec3f(v*hRGB[0], v*hRGB[1], v*hRGB[2]);
        }

        
        @Override
        public float getMeanEquilibriumDistance() {
            return (float)getSliderValue("MeanEquilibriumDistance");
            //return 2.0f;
        }
                
        @Override
        public float getVertexEquilibriumDistance(Atom n) {
            final double sti = mind.getNormalizedSTI(n);
            return getMeanEquilibriumDistance() / ((float)sti + 1.0f);
        }

        @Override
        public double getLayoutUpdatePeriod() {
            return 0.1;
        }

        @Override
        public float getInterpolationMomentum() {
            return 0.8f;
        }

        @Override
        public int getMaxAtoms() {
            return 128;
        }

        @Override
        public boolean getUpdateGraph() {
            if (updateGraph) {
                updateGraph = false;
                return true;
            }
            return false;
        }
        
        
        
    }
    

//    @Override
//    public String getName() {
//        return "GraphView1";
//    }
//
//    @Override
//    public String getDescription() {
//        return "Graph View";
//    }
    
    @Deprecated public GraphView(final OCMind mind, GraphViewProcess... p) {
        this(mind, new SeHGraphViewModel1(mind), p);
    }

    public GraphView(OCMind mind, GraphViewModel param, GraphViewProcess... p) {
        super();
        
        this.mind = mind;
        this.param = param;
        
        //add(new GridRect(6, 6));

        add(this);
        
        add(new PointerLayer(this, 2));

        updateGraph();

        for (GraphViewProcess gvp : p)
            processes.add(gvp);
        
//        processes.add(new GraphViewProcess() {
//
//            @Override
//            protected void update(GraphView g) {
//                System.out.println("updating graph");
//                GraphView.this.updateGraph();
//            }
//
//            @Override
//            public boolean isReady() {
//                return getAccumulated() > 1;
//            }
//            
//        });
        
        processes.add(new HyperassociativeLayoutProcess());
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
    
    private void updateRect(Atom vertex, TextRect r) {
        final Vec2f s = param.getVertexScale(vertex, maxSTI, minSTI);
        setTargetScale(r, s.x(), s.y(), 1.0f);
        r.setBackgroundColor(param.getVertexColor(vertex));
        r.setFilled(true);
    }
    
    protected Vec3f newInitialPosition(float r, float z) {
//        final float z = 0.5f;
//        float r = 4.0f;
        return new Vec3f(RandomNumber.getFloat(-r, r), RandomNumber.getFloat(-r, r), z);
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

        public FoldedEdge(final Atom src, final Atom dest, final Atom parentEdge, final String label) {
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
        if (c == null)
            return;
        
        final Atom a = e.parentEdge;
        
        c.setWidths(param.getCurveProfile(a));        
        c.setColor(param.getCurveColor(a));        
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
    public MutableDirectedAdjacencyGraph<Atom, FoldedEdge> foldHypergraphEdges(Collection<Atom> vertices, final MutableDirectedAdjacencyGraph<Atom, FoldedEdge> target, final Hypergraph<Atom,Atom> h, boolean linkEdgeToMembers)
    {
        for (Atom v : vertices) {
            target.add(v);            
        }
        
        for (Atom e : h.getEdges())
        {
            boolean contained = true;
            for (Atom iv : mind.getIncidentVertices(e)) {
                if (!vertices.contains(iv)) {
                    contained = false;
                    break;                    
                }
            }
            
            if (!contained)
                continue;
            
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

        atomRect.clear();
        edgeCurve.clear();
        
        List<Atom> arank = mind.getAtomsBySTI(true);        
        
        int n = Math.min(arank.size(), param.getMaxAtoms());
        
        List<Atom> highest = arank.subList(0, n);
                
        System.out.println("Updating graph: " + n + " out of " + arank.size() + " total atoms");
        
        
        Set<Atom> hm = new HashSet(highest);
        
        for (Atom v : hm ) {
            addVertex(v);
        }        
        
        digraph = foldHypergraphEdges(hm, new MutableDirectedAdjacencyGraph<Atom, FoldedEdge>(),
                mind.atomspace.graph, false);
        
        for (FoldedEdge e : new LinkedList<FoldedEdge>(digraph.getEdges())) {
             addEdge(e);
        }                  
        
        for (GraphViewProcess gvp : processes)
            gvp.reset();
    }
    
    public abstract static class GraphViewProcess {
        double accumulated = 0;
        
        public void _update(GraphView g) {
            accumulated = 0;
            update(g);
        }
        
        public void reset() { }
        
        abstract protected void update(GraphView g);
        
        abstract public boolean isReady();
        
        public void accumulate(double dt) {
            accumulated += dt;
        }
        
        public double getAccumulated() {
            return accumulated;
        }
    }
    
    public Map<Spatial, Vec3f> targetCenter = new WeakHashMap();
    public Map<Spatial, Vec3f> targetScale = new WeakHashMap();
                
    public void setTargetCenter(Spatial s, float x, float y, float z) {
        Vec3f v = targetCenter.get(s);
        if (v == null) {
            v = new Vec3f(x, y, z);
            targetCenter.put(s, v);
        }
        else
            v.set(x,y,z);
    }
    public void setTargetScale(Spatial s, float x, float y, float z) {
        Vec3f v = targetScale.get(s);
        if (v == null) {
            v = new Vec3f(x, y, z);
            targetScale.put(s, v);
        }
        else
            v.set(x,y,z);
    }
    
    public class HyperassociativeLayoutProcess extends GraphViewProcess {

        final int alignCycles = 1;
        private SeHHyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, FoldedEdge>, Atom> ham;

        public HyperassociativeLayoutProcess() {
            
            reset();
        }

        @Override
        public void reset() {
            super.reset();
            
            int numDimensions = 2;
        
            ham = new SeHHyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, FoldedEdge>, Atom>(digraph, numDimensions, true, executor) {

                @Override
                public float getEquilibriumDistance(Atom n) {
                    return param.getVertexEquilibriumDistance(n);
                }

                @Override
                public float getMeanEquilibriumDistance() {
                    return param.getMeanEquilibriumDistance();
                }

            };
        }
        
        
        
        @Override
        protected void update(GraphView g) {
            
            for (int i = 0; i < alignCycles; i++)
                ham.align();


            final float s = 0.2f;
            for (Entry<Atom,TextRect> i : g.atomRect.entrySet()) {
                final Vector v = ham.getCoordinates().get(i.getKey());
                
                TextRect tr = i.getValue();
                if (v.getDimensions() == 2) {
                    float x = (float)v.getCoordinate(1)*s;
                    float y = (float)v.getCoordinate(2)*s;
                    //i.getValue().setCenter(x, y);
                    setTargetCenter(tr, x, y, 0);
                }
                else if (v.getDimensions() == 3) {
                    float x = (float)v.getCoordinate(1)*s;
                    float y = (float)v.getCoordinate(2)*s;
                    float z = (float)v.getCoordinate(3)*s;
                    //i.getValue().setCenter(x, y, z);
                    setTargetCenter(tr, x, y, z);
                }
            }
        }

        @Override
        public boolean isReady() {
            return accumulated > param.getLayoutUpdatePeriod();
        }
        
    }
    
    final List<GraphViewProcess> processes = new LinkedList();
    
    protected void layoutGraph() {
        final double dt = getDT();
        for (GraphViewProcess p : processes) {
            if (p.isReady())
                p._update(this);
            else
                p.accumulate(dt);
        }

        for (Atom v : atomRect.keySet()) {
            updateRect(v, atomRect.get(v));
        }
        for (FoldedEdge e : digraph.getEdges()) {
            updateCurve(e);            
        }
        
        
        //interpolate
        final float momentum = param.getInterpolationMomentum();
        for (Entry<Spatial, Vec3f> e : targetCenter.entrySet()) {
            final Spatial s = e.getKey();
            final Vec3f v = e.getValue();
            s.getCenter().lerp(v, momentum);
            
            final Vec3f vs = targetScale.get(s);
            s.getScale().lerp(vs, momentum);
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
    
    public double getDT() { return (currentTime - lastTime)/1.0e9; }
    
    @Override
    public void draw(GL2 gl) {
        lastTime = currentTime;
        currentTime = System.nanoTime();

        if (param.getUpdateGraph()) {
            updateGraph();
        }
        
        boolean first = true;
        for (Atom a : digraph.getNodes()) {
            if (first) {
                minSTI = maxSTI = mind.getSTI(a);
                first = false;
            }
            else {
                short as = mind.getSTI(a);
                if (as < minSTI) minSTI = as;
                if (as > maxSTI) maxSTI = as;
            }
        }
                
        layoutGraph();
        drawAtoms(gl);
        drawEdges(gl);
    }
        
    public static GraphPanel newGraphPanel(OCMind mind, GraphViewProcess... p) {
        return new GraphPanel(new GraphView(mind, p));
    }

    public static void newGraphWindow(OCMind mind, GraphViewProcess... p) {
        new SwingWindow(newGraphPanel(mind, p), 800, 800, true);
    }



}
