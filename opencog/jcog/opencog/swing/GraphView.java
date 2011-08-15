/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing;

import jcog.opencog.swing.graph.FoldedEdge;
import jcog.opencog.swing.graph.GraphViewProcess;
import com.sun.opengl.util.awt.TextRenderer;
import edu.uci.ics.jung.graph.util.Pair;
import java.awt.Color;
import java.awt.Font;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.media.opengl.GL2;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JToggleButton;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.AtomType.UnorderedLink;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import jcog.opencog.atom.TruthValue;
import jcog.opencog.swing.graph.HyperassociativeLayoutProcess;
import jcog.opencog.util.AtomTypes;
import jcog.spacegraph.gl.Surface;
import jcog.spacegraph.math.linalg.Vec3f;
import jcog.spacegraph.math.linalg.Vec4f;
import jcog.spacegraph.shape.Drawable;
import jcog.spacegraph.shape.Spatial;
import jcog.spacegraph.shape.TextRect;
import jcog.spacegraph.shape.TrapezoidLine;
import jcog.spacegraph.swing.SwingWindow;
import jcog.spacegraph.ui.PointerLayer;
import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;

/**
 *
 * @author seh
 */
public class GraphView extends Surface implements Drawable {

    final public static ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
    public final ConcurrentHashMap<Atom, TextRect> atomRect = new ConcurrentHashMap();
    public final ConcurrentHashMap<FoldedEdge, TrapezoidLine> edgeCurve = new ConcurrentHashMap();
    //TODO
    //TODO
    // remove entries from these maps when an object disappears, or store them in the shape objects
    //TODO
    //TODO    
    public final Map<Spatial, Vec3f> targetCenter = new ConcurrentHashMap();
    public final Map<Spatial, Vec3f> targetScale = new ConcurrentHashMap();
    private final OCMind mind;
    final static TextRenderer textRenderer = TextRect.newTextRenderer(new Font("Arial", Font.PLAIN, 32));
    public final GraphViewModel param;
    private long currentTime = 0;
    private long lastTime = 0;
    private short minSTI, maxSTI;

    public static interface GraphViewModel {

        public void updateVertexColor(Atom vertex, Vec4f vec);

        public void updateVertexScale(Atom v, short maxSTI, short minSTI, Vec3f vec);

        public float[] getCurveProfile(Atom edge);

        public void updateCurveColor(Atom edge, Vec3f vec);

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
        //private final Atom parameterNode;
        private final MindAgent ma;

        public SeHGraphViewModel1(final OCMind mind) {
            super();
            this.mind = mind;

            ma = new MindAgent(getGraphProcessPeriod()) {

                @Override
                protected void run(OCMind mind) {
                    for (String s : parameters.keySet()) {
                        Atom v = parameters.get(s).getSecond();
                        mind.setName(v, Double.toString(getSliderValue(s)));

                    }
//                    if (mind.getSTI(parameterNode) > 0) {
//                        addStimulus(parameterNode, (short) -DECREASE_STI);
//                    }

                }
            };

            mind.addAgent(ma);

            //parameterNode = mind.addVertex(AtomType.conceptNode, this.toString());

            control.setLayout(new BoxLayout(control, BoxLayout.PAGE_AXIS));
            {
//                JButton updateButton = new JButton("Update");
//                updateButton.addActionListener(new ActionListener() {
//
//                    @Override
//                    public void actionPerformed(ActionEvent e) {
//                        //updateGraph();                        
//                    }
//                });
//                control.add(updateButton);

                addSlider("AutoUpdateHz", 2.0, 0, 3.0);
                addSlider("MaxAtoms", 128, 1, 512);
                addSlider("MeanEquilibriumDistance", 2.0, 0.1, 6.0);
                addSlider("VertexScale", 0.2, 0.1, 1.0);
                addSlider("EdgeWidthScale", 0.05, 0.001, 0.2);
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
        Map<String, JSlider> sliders = new HashMap();

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

        double getSliderValue(final String method) {
            return fromSlider(sliders.get(method).getValue());
        }

        @Override
        public void updateVertexColor(Atom v, Vec4f vec) {
            final float sti = (float) mind.getNormalizedSTI(v);

            String n = mind.getName(v);
            if (n == null) {
                String type = mind.getTypeName(v);
                if (type != null) {
                    n = type.toString();
                } else {
                    n = "UNKNOWN";
                }
            }

            final float hue = ((Math.abs((n.toString().hashCode()))) % 100) / 100.0f;

            final Color h = Color.getHSBColor(hue, 0.85f, sti * 0.5f + 0.5f);
            float[] hRGB = h.getColorComponents(null);
            vec.set(hRGB[0], hRGB[1], hRGB[2], 1.0f);
        }

        @Override
        public void updateVertexScale(Atom v, short maxSTI, short minSTI, Vec3f vec) {
            final float vertexScale = (float) getSliderValue("VertexScale");
            final double sti = mind.getNormalizedSTI(v, maxSTI, minSTI);
            float sx = 0.1f + (float) (sti) * vertexScale;
            vec.set(sx, sx, 1.0f);
        }

        @Override
        public float[] getCurveProfile(Atom edge) {
            float rti = (float) mind.getNormalizedSTI(edge);
            final TruthValue tv = mind.getTruth(edge);
            final float w = 1f + (float) tv.getMean() * 4f;
            final float edgeWidthScale = (float) getSliderValue("EdgeWidthScale") * (1.0f + rti);
            final float edgeRatio = UnorderedLink.class.isAssignableFrom(mind.getType(edge)) ? 1.0f : 3.0f;

            return new float[]{w * edgeWidthScale, w * (edgeWidthScale / edgeRatio)};
        }

        @Override
        public void updateCurveColor(Atom edge, Vec3f vec) {
            final float v = 0.7f + 0.3f * (float) mind.getTruth(edge).getMean();

            String type = mind.getTypeName(edge);

            final float hue = ((Math.abs(type.hashCode())) % 100) / 100.0f;

            final Color h = Color.getHSBColor(hue, 0.85f, v);
            float[] hRGB = h.getColorComponents(null);
            vec.set(v * hRGB[0], v * hRGB[1], v * hRGB[2]);
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
        public double getLayoutUpdatePeriod() {
            return 0.07;
        }

        @Override
        public float getInterpolationMomentum() {
            return 0.8f;
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
            return 0.05;
        }

        @Override
        public Predicate<Atom> getIncludedAtoms() {
            return includedAtoms;
        }
    }

    public class GraphViewUpdate extends MindAgent {

        public GraphViewUpdate() {
            super(0);
        }

        @Override
        protected void run(OCMind mind) {
            if (getPeriod() > 0) {
                updateGraph();
            }
        }
    }
    GraphViewUpdate graphViewUpdate;

    @Deprecated
    public GraphView(final OCMind mind, GraphViewProcess... p) {
        this(mind, new SeHGraphViewModel1(mind), p);
    }

    public GraphView(OCMind mind, final GraphViewModel param, GraphViewProcess... p) {
        super();

        this.mind = mind;
        this.param = param;

        //add(new GridRect(6, 6));

        add(this);

        add(new PointerLayer(this, 2));

        for (GraphViewProcess gvp : p) {
            processes.add(gvp);
        }

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

        mind.addAgent(new MindAgent() {

            @Override
            protected void run(OCMind mind) {
                for (GraphViewProcess p : processes) {
                    if (p.isReady()) {
                        p._update(GraphView.this);
                    } else {
                        p.accumulate(getDT());
                    }
                    graphViewUpdate.setPeriod(param.getGraphUpdatePeriod());
                }
            }
        }).setPeriod(param.getGraphProcessPeriod());

        graphViewUpdate = new GraphViewUpdate();
        mind.addAgent(graphViewUpdate);


        processes.add(new HyperassociativeLayoutProcess(this));
        //processes.add(new FDLayoutProcess(this));
    }

    protected void addVertex(final Atom v) {
        TextRect r = atomRect.get(v);
        if (r == null) {
            String name = mind.getName(v);
            if (name == null) {
                name = "";
            }

            r = new TextRect(textRenderer, name);
            //r.setnewInitialPosition(), newInitialScale());
            float rr = 10f;
            r.getCenter().set(RandomNumber.getFloat(-rr, rr), RandomNumber.getFloat(-rr, rr), 0);
            atomRect.put(v, r);
        }
    }

    private void updateRect(Atom vertex, TextRect r) {
        param.updateVertexScale(vertex, maxSTI, minSTI, getTargetScale(r));
        param.updateVertexColor(vertex, r.getBackgroundColor());
        r.setFilled(true);
    }

    protected Vec3f newInitialPosition(float r, float z) {
//        final float z = 0.5f;
//        float r = 4.0f;
        return new Vec3f(RandomNumber.getFloat(-r, r), RandomNumber.getFloat(-r, r), z);
    }

    public void addEdge(final FoldedEdge e) {
        final Atom s = e.getSourceNode();
        final Atom t = e.getDestinationNode();
        TrapezoidLine c = edgeCurve.get(e);
        if (c == null) {
            c = new TrapezoidLine(atomRect.get(s), atomRect.get(t), 0.1f, 0.05f);
            edgeCurve.put(e, c);
        }
    }

    private void updateCurve(FoldedEdge e) {
        final TrapezoidLine c = edgeCurve.get(e);
        if (c == null) {
            return;
        }

        final Atom a = e.parentEdge;

        c.setWidths(param.getCurveProfile(a));

        param.updateCurveColor(a, c.getColor());
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
    protected void updateGraph() {

        int remained = 0, removed = 0, added = 0;

        List<Atom> arank = IteratorUtils.toList(mind.iterateAtomsByDecreasingSTI(param.getIncludedAtoms()));

        int n = Math.min(arank.size(), param.getMaxAtoms());

        List<Atom> highest = arank.subList(0, n);


        Set<Atom> hm = new HashSet(highest);

        List<Atom> rectsToRemove = new LinkedList();

        for (Atom v : atomRect.keySet()) {
            if (!hm.contains(v)) {
                rectsToRemove.add(v);
                removed++;
            } else {
                remained++;
            }
        }

        for (Atom v : hm) {
            if (!atomRect.containsKey(v)) {
                addVertex(v);
                added++;
            }
        }
        for (Atom a : rectsToRemove) {
            atomRect.remove(a);
        }

        //System.out.println("Updated graph: " + n + " out of " + arank.size() + " total atoms; " + remained + ", " + removed + ", " + added);


        if ((added > 0) || (removed > 0)) {
            for (GraphViewProcess gvp : processes) {
                gvp.reset();
            }
        }

        //System.out.println("  Updated graph: " + n + " out of " + arank.size() + " total atoms; " + remained + ", " + removed + ", " + added);

    }

    public void setTargetCenter(Spatial s, float x, float y, float z) {
        Vec3f v = targetCenter.get(s);
        if (v == null) {
            v = new Vec3f(x, y, z);
            targetCenter.put(s, v);
        } else {
            v.set(x, y, z);
        }
    }

    public Vec3f getTargetScale(Spatial s) {
        Vec3f v = targetScale.get(s);
        if (v == null) {
            v = new Vec3f(0, 0, 0);
            targetScale.put(s, v);
        }
        return v;
    }

    public void setTargetScale(Spatial s, float x, float y, float z) {
        Vec3f v = targetScale.get(s);
        if (v == null) {
            v = new Vec3f(x, y, z);
            targetScale.put(s, v);
        } else {
            v.set(x, y, z);
        }
    }

    public OCMind getMind() {
        return mind;
    }
    final List<GraphViewProcess> processes = new LinkedList();

    protected void layoutGraph() {

        for (Atom v : atomRect.keySet()) {
            if (mind.hasAtom(v)) {
                TextRect r = atomRect.get(v);
                if (r != null) {
                    updateRect(v, r);
                } else {
                    System.out.println("failed to remove atom: " + v);
                }
            }
            else {
                //atomRect.remove(v);
            }
        }
        for (FoldedEdge e : edgeCurve.keySet()) {
            updateCurve(e);
        }


        //interpolate
        final float momentum = param.getInterpolationMomentum();
        for (Entry<Spatial, Vec3f> e : targetCenter.entrySet()) {
            final Spatial s = e.getKey();
            final Vec3f v = e.getValue();
            s.getCenter().lerp(v, momentum);


            final Vec3f vs = targetScale.get(s);
            if (vs != null) {
                s.getScale().lerp(vs, momentum);
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

    public double getDT() {
        return (currentTime - lastTime) / 1.0e9;
    }

    @Override
    public void draw(GL2 gl) {
        lastTime = currentTime;
        currentTime = System.nanoTime();

        boolean first = true;
        
        for (Atom a : atomRect.keySet()) {
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

        layoutGraph();
        drawAtoms(gl);
        drawEdges(gl);

    }

}
