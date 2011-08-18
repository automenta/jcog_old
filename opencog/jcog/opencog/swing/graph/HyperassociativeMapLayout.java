/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import com.syncleus.dann.graph.drawing.hyperassociativemap.HyperassociativeMap;
import com.syncleus.dann.math.Vector;
import java.util.Collection;
import java.util.Map.Entry;
import jcog.opencog.Atom;
import jcog.opencog.swing.GraphView2D;
import jcog.spacegraph.shape.Rect;
import org.encog.util.ParamsHolder;

/**
 *
 * @author seh
 */
public class HyperassociativeMapLayout extends GraphViewProcess {
    final int alignCycles = 1;
    final int numDimensions = 2;
    private MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment> digraph;
    //private SeHHyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, HyperedgeSegment>, Atom> ham;
    private HyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, HyperedgeSegment>, Atom> ham;
    private Atom selected = null;
    
    public HyperassociativeMapLayout() {
        super();
    }


    public void setSelected(Atom selected) {
        this.selected = selected;
    }
    
    @Override
    public void reset(final GraphView2D graphView) {
        if (digraph == null)
            return;
        
        digraph = mind.foldHypergraphEdges(graphView.atomRect.keySet(), new MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment>(), true);
        Collection<HyperedgeSegment> diEdges = digraph.getEdges();

        graphView.edgeCurve.clear();
        for (HyperedgeSegment fe : diEdges) {
            graphView.addEdge(fe);
        }
        
        ham = new HyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, HyperedgeSegment>, Atom>(digraph, numDimensions, GraphView2D.executor) {

            @Override
            public double getEquilibriumDistance() {
                return graphView.param.getMeanEquilibriumDistance()*10.0;
            }
//
//            @Override
//            public float getEquilibriumDistance(Atom n) {
//                return graphView.param.getVertexEquilibriumDistance(n);
//            }
//
//            @Override
//            public float getMeanEquilibriumDistance() {
//                return graphView.param.getMeanEquilibriumDistance();
//            }
            
        };
        //ham.setLearningRate(0.8);
        
        for (Atom a : graphView.atomRect.keySet()) {
            Rect r = graphView.atomRect.get(a);
            final float x = r.getCenter().x();
            final float y = r.getCenter().y();
            final float z = r.getCenter().z();
            if (numDimensions >= 2) {
                ham.getCoordinates().get(a).setCoordinate(x, 1);
                ham.getCoordinates().get(a).setCoordinate(y, 2);
            }
            if (numDimensions >= 3) {
                ham.getCoordinates().get(a).setCoordinate(z, 3);
            }
            graphView.setTargetCenter(r, x, y, z);
        }
    }

    @Override
    protected void update(GraphView2D g) {
        if (ham == null) {
            return;
        }

        if (selected!=null) {
            double minZ = -5;
            Vector v = ham.getCoordinates().get(selected);
            if (v==null) {
                v = new Vector(ham.getDimensions());
                ham.getCoordinates().put(selected, v);
            }
            v.multiply(0.9); //head to 0,0,0
        }
        
        for (int i = 0; i < alignCycles; i++) {
            ham.align();
        }
        final float s = 0.2F;
        for (Entry<Atom, Rect> i : g.atomRect.entrySet()) {
            final Vector v = ham.getCoordinates().get(i.getKey());
            if (v == null) {
                System.err.println(i + " not mapped by " + this);
            }
            Rect tr = i.getValue();
            if (v.getDimensions() == 2) {
                float x = (float) v.getCoordinate(1) * s;
                float y = (float) v.getCoordinate(2) * s;
                //i.getValue().setCenter(x, y);
                g.setTargetCenter(tr, x, y, 0);
            } else if (v.getDimensions() == 3) {
                float x = (float) v.getCoordinate(1) * s;
                float y = (float) v.getCoordinate(2) * s;
                float z = (float) v.getCoordinate(3) * s;
                //i.getValue().setCenter(x, y, z);
                g.setTargetCenter(tr, x, y, z);
            }
        }
    }

    @Override
    public boolean isReady(GraphView2D g) {
        return accumulated > g.param.getLayoutUpdatePeriod();
    }
    
}
