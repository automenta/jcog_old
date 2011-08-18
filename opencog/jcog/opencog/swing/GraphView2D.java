/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing;

import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import jcog.opencog.swing.graph.GraphView2DRenderer;
import jcog.opencog.swing.graph.HyperedgeSegment;
import jcog.opencog.swing.graph.GraphViewProcess;
import edu.uci.ics.jung.graph.util.Pair;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import javax.media.opengl.GL2;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JToggleButton;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import jcog.opencog.util.AtomTypes;
import jcog.spacegraph.gl.Surface;
import jcog.spacegraph.math.linalg.Vec3f;
import jcog.spacegraph.shape.Drawable;
import jcog.spacegraph.shape.Rect;
import jcog.spacegraph.shape.Spatial;
import jcog.spacegraph.shape.TrapezoidLine;
import jcog.spacegraph.swing.SwingWindow;
import jcog.spacegraph.ui.PointerLayer;
import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;

/**
 *
 * @author seh
 */
public class GraphView2D extends Surface implements Drawable {

    private final OCMind mind;    
    public final GraphViewModel param;
    private final GraphView2DRenderer renderer;

    private MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment> digraph;
    
    private final Map<Atom, Rect> vertexShape = new ConcurrentHashMap();
    private final Map<HyperedgeSegment, TrapezoidLine> edgeShape = new ConcurrentHashMap();
    private final Map<Spatial, Pair<Vec3f>> targetShape = new ConcurrentHashMap(); //1st=center. 2nd=scale

    private final List<GraphViewProcess> processes = new LinkedList();
        
    private long currentTime = 0;
    private long lastTime = 0;
    
    private short minSTI, maxSTI;

    public MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment> getDiGraph() {
        return digraph;
    }

    public static interface GraphViewModel {

        public double getDouble(String parameter);
        
        public float getVertexEquilibriumDistance(Atom n);

        public float getMeanEquilibriumDistance();

        public double getLayoutUpdatePeriod();

        public float getInterpolationMomentum();

        public int getMaxAtoms();

        public double getGraphUpdatePeriod();

        public double getGraphProcessPeriod();

        public Predicate<Atom> getIncludedAtoms();
    }

    public static class SeHGraphViewModel1 implements GraphViewModel {

        final int integerScale = 200;
        private short PARAMETER_CHANGE_BOOST = 0;
        private short DECREASE_STI = 0;
        private final OCMind mind;
        private final JPanel control = new JPanel();
        private final Predicate<Atom> includedAtoms;
        private final Map<String, Pair<Atom>> parameters = new HashMap();
        Map<String, JSlider> sliders = new HashMap();

        public SeHGraphViewModel1(final OCMind mind) {
            super();
            this.mind = mind;

            control.setLayout(new BoxLayout(control, BoxLayout.PAGE_AXIS));
            {
//                JButton updateButton = new JButton("Update");
//                updateButton.addActionListener(new ActionListener() {
//                    @Override
//                    public void actionPerformed(ActionEvent e) {
//                        refreshGraph(false);
//                    }
//                });
//                control.add(updateButton);

                addSlider("AutoUpdateHz", 0.25, 0, 3.0);
                addSlider("MaxAtoms", 128, 1, 512);
                addSlider("MeanEquilibriumDistance", 4.0, 0.1, 6.0);
                addSlider("VertexScale", 0.2, 0.1, 1.0);
                addSlider("EdgeWidthScale", 0.01, 0.001, 0.1);
            }

            final List<Class<? extends AtomType>> types = AtomTypes.getTypes();
            final Map<Class<? extends AtomType>, JToggleButton> typeEnables = new HashMap();
            {
                for (Class<? extends AtomType> ca : types) {
                    JToggleButton jtb = new JToggleButton(ca.getSimpleName());
                    jtb.setSelected(true);
                    typeEnables.put(ca, jtb);
                    control.add(jtb);
                }
            }

            control.add(Box.createVerticalBox());

            includedAtoms = new Predicate<Atom>() {

                @Override
                public boolean evaluate(Atom t) {
                    final Class<? extends AtomType> tt = mind.getType(t);
                    for (final Class<? extends AtomType> ca : types) {
                        if (ca.isAssignableFrom(tt) && typeEnables.get(ca).isSelected()) {
                            return true;
                        }
                    }
                    return false;
                }
            };

            new SwingWindow(new JScrollPane(control), 400, 500);

        }

        public int toSlider(double v) {
            return (int) (v * integerScale);
        }

        public double fromSlider(int x) {
            return (double) x / ((double) integerScale);
        }

        public JSlider addSlider(String methodSuffix, double initialValue, double min, double max) {

            final JSlider js = new JSlider(JSlider.HORIZONTAL);
            js.setMinimum(toSlider(min));
            js.setMaximum(toSlider(max));
            js.setValue(toSlider(initialValue));

            sliders.put(methodSuffix, js);

            control.add(new JLabel(methodSuffix));
            control.add(js);

            /*
            final Atom a = mind.addVertex(AtomType.conceptNode, methodSuffix);
            mind.addEdge(AtomType.extensionalInheritanceLink, parameterNode, a);

            final Atom v = mind.addVertex(AtomType.conceptNode, Double.toString(getSliderValue(methodSuffix)));
            mind.addEdge(AtomType.evaluationLink, a, v);
             * 
             */
//            parameters.put(methodSuffix, new Pair<Atom>(a, v));

//            js.addChangeListener(new ChangeListener() {
//
//                @Override
//                public void stateChanged(ChangeEvent e) {
//                    ma.addStimulus(a, PARAMETER_CHANGE_BOOST);
//                }
//            });

            return js;
        }

        double getSliderValue(final String parameter) {
            return fromSlider(sliders.get(parameter).getValue());
        }

        @Override
        public double getDouble(final String parameter) {
            return getSliderValue(parameter);
        }



        @Override
        public float getMeanEquilibriumDistance() {
            return (float) getSliderValue("MeanEquilibriumDistance");
            //return 2.0f;
        }

        @Override
        public float getVertexEquilibriumDistance(Atom n) {
            //TODO use a log curve to make it feel more linear
//            final double sti = mind.getNormalizedSTI(n);
//            return getMeanEquilibriumDistance() / ((float) sti + 1.0f);
            return getMeanEquilibriumDistance();
        }


        @Override
        public float getInterpolationMomentum() {
            return 0.9f;
        }

        @Override
        public int getMaxAtoms() {
            return (int) getSliderValue("MaxAtoms");
        }

        public double getGraphUpdatePeriod() {
            double p = getSliderValue("AutoUpdateHz");
            if (p == 0) {
                return 0;
            }
            return 1.0 / p;
        }

        @Override
        public double getGraphProcessPeriod() {
            return 0.01;
        }
        @Override
        public double getLayoutUpdatePeriod() {
            return 0.05;
        }

        @Override
        public Predicate<Atom> getIncludedAtoms() {
            return includedAtoms;
        }
    }
  
    
    public GraphView2D(OCMind mind, final GraphView2DRenderer renderer, final GraphViewModel param, GraphViewProcess... p) {
        super();

        this.mind = mind;
        this.param = param;
        this.renderer = renderer;

        add(this);

        add(new PointerLayer(this, 2));

        for (GraphViewProcess gvp : p) {
            processes.add(gvp);
        }

        refreshGraph(true);
        
        mind.addAgent(new MindAgent(param.getGraphProcessPeriod()) {

            @Override
            protected void run(OCMind mind) {
                //timestamp("update: " + getPeriod());
                for (final GraphViewProcess p : processes) {
                    if (p.isReady(GraphView2D.this)) {
                        p._update(GraphView2D.this);
                    } else {
                        p.accumulate(getDT());
                    }
                }
                updateRenderables();
            }
        });

        //THIS IS TEMPORARY
        mind.addAgent(new MindAgent(4.0) {
            @Override
            protected void run(OCMind mind) {
                refreshGraph(false);
            }            
        });
    }
    

    public Rect addVertex(final Atom v) {
        Rect r = vertexShape.get(v);
        if (r == null) {
            String name = mind.getName(v);
            if (name == null) {
                name = "";
            }

            r = renderer.newVertex(mind, v);
                       
            vertexShape.put(v, r);
            
            targetShape.put(r, new Pair<Vec3f>(r.getCenter(), r.getScale()));
        
        }
        else {
            System.err.println("do not call addVertex for already added atoms like " + v);
        }
        return r;
    }
    
    public TrapezoidLine addEdge(final HyperedgeSegment e) {
        final Atom s = e.getSourceNode();
        final Atom t = e.getDestinationNode();
        TrapezoidLine c = edgeShape.get(e);
        if (c == null) {
            c = renderer.newEdge(mind, e.parentEdge, s, t, vertexShape.get(s), vertexShape.get(t));
            edgeShape.put(e, c);
        }
        else {
            System.err.println("do not call addEdge for already added edges like " + e);            
        }
        return c;
    }

    public void removeVertex(final Atom v) {
        if (!vertexShape.containsKey(v)) {
            System.err.println("do not call removeVertex for non-existent vertexes like " + v);
            return;
        }
        targetShape.remove(vertexShape.get(v));
        vertexShape.remove(v);
    }
    
    public void removeEdge(final HyperedgeSegment e) {
        final Atom v = e.parentEdge;
        if (!edgeShape.containsKey(e)) {
            System.err.println("do not call removeEdge for non-existent edges like " + e + " " + v);
            return;
        }
        edgeShape.remove(e);
    }

    private void updateVertex(Atom vertex) {
        if (mind.containsAtom(vertex))           
            renderer.updateVertex(this, vertex, getVertexShape(vertex));
        else {
            //the atom has been removed since last refreshGraph
        }
    }
    private void updateEdge(HyperedgeSegment e) {
        if (mind.containsAtom(e.parentEdge)) {
            renderer.updateEdge(this, e.parentEdge, getEdgeShape(e));
        }
        else {
            //the atom has been removed since last refreshGraph
        }
    }


    protected void refreshGraph(boolean forceRefreshs) {

        int remained = 0, removed = 0, added = 0;

        List<Atom> arank = IteratorUtils.toList(mind.iterateAtomsByDecreasingSTI(param.getIncludedAtoms()));

        final int n = Math.min(arank.size(), param.getMaxAtoms());

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

        if ((added > 0) || (removed > 0) || (forceRefreshs)) {
            for (GraphViewProcess gvp : processes) {
                gvp.refresh(this);
            }
        }
        
        //System.out.println((getVisibleVertices().size()) + " " + getDiGraph().getNodes().size() + " " + targetShape.size());
        //System.out.println("  " + (getVisibleEdges().size()) + " " + getDiGraph().getEdges().size());

        updateMinMaxSTI();        
    }

    public void setTargetCenter(Spatial s, float x, float y, float z) {
        targetShape.get(s).getFirst().set(x, y, z);
    }

    public Vec3f getTargetScale(Spatial s) {
        return targetShape.get(s).getSecond();
    }

    public void setTargetScale(Spatial s, float x, float y, float z) {
        targetShape.get(s).getSecond().set(x, y, z);
    }

    public OCMind getMind() {
        return mind;
    }
    

    protected void updateRenderables() {
        for (final Atom v : getVisibleVertices()) {
            updateVertex(v);
        }
        for (final HyperedgeSegment e : getVisibleEdges()) {
            updateEdge(e);
        }        
    }
    
    public static void timestamp(String msg) {
        double t = ((double)System.nanoTime()) / 1.0e9;
        System.out.println(t + ": " + msg);
    }
    protected void interpolate() {
        final float momentum = param.getInterpolationMomentum();

        //interpolate
        for (final Entry<Spatial, Pair<Vec3f>> e : targetShape.entrySet()) {
            final Spatial s = e.getKey();
            
            final Vec3f pos = e.getValue().getFirst();
            final Vec3f vs = e.getValue().getSecond();
            
            s.getCenter().lerp(pos, momentum);
            s.getScale().lerp(vs, momentum);
            s.updateGeometry();
        }
    }

    protected void drawAtoms(GL2 gl) {        
        for (final Drawable d : vertexShape.values()) {
            d.draw(gl);
        }
    }

    protected void drawEdges(GL2 gl) {
        for (final Drawable d : edgeShape.values()) {
            d.draw(gl);
        }
    }

    public double getDT() {
        return (currentTime - lastTime) / 1.0e9;
    }

    public Collection<Atom> getVisibleVertices() {
        return Collections.unmodifiableCollection(vertexShape.keySet());
    }
    public Collection<HyperedgeSegment> getVisibleEdges() {
        return Collections.unmodifiableCollection(edgeShape.keySet());
    }
    
    protected void updateMinMaxSTI() {
        boolean first = true;        
        for (final Atom a : getVisibleVertices()) {
            if (first) {
                minSTI = maxSTI = mind.getSTI(a);
                first = false;
            } else {
                short as = mind.getSTI(a);
                if (as < minSTI) {
                    minSTI = as;
                }
                if (as > maxSTI) {
                    maxSTI = as;
                }
            }
        }        
    }
    
    @Override
    public void draw(GL2 gl) {
        lastTime = currentTime;
        currentTime = System.nanoTime();

        interpolate();
        drawAtoms(gl);
        drawEdges(gl);
    }
    
    public Rect getVertexShape(final Atom a) {
        return vertexShape.get(a);
    }
    
    public TrapezoidLine getEdgeShape(final HyperedgeSegment a) {
        return edgeShape.get(a);
    }


}
