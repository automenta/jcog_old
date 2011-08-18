/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import com.syncleus.dann.graph.drawing.hyperassociativemap.HyperassociativeMap;
import com.syncleus.dann.math.Vector;
import java.util.Map.Entry;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import jcog.opencog.Atom;
import jcog.opencog.swing.GraphView2D;
import jcog.spacegraph.shape.Rect;

/**
 *
 * @author seh
 */
public class HyperassociativeMapLayout extends GraphViewProcess {
    final public static ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
    
    final int alignCycles = 1;
    final int numDimensions = 2;
    float hamScale = 0.2F;
    
    //private SeHHyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, HyperedgeSegment>, Atom> ham;
    private HyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, HyperedgeSegment>, Atom> ham;
    
    public HyperassociativeMapLayout() {
        super();
    }
    
    @Override
    public void refresh(final GraphView2D graphView) {
        
        ham = new HyperassociativeMap<com.syncleus.dann.graph.Graph<Atom, HyperedgeSegment>, Atom>(
                graphView.getDiGraph(), numDimensions, executor) {

            @Override
            public double getEquilibriumDistance() {
                return graphView.param.getMeanEquilibriumDistance()*10.0;
            }
            
        };
        //ham.setLearningRate(0.8);
        
        shapesToHAM(graphView);
    }
    
    protected void shapesToHAM(final GraphView2D graphView) {
        for (final Atom a : graphView.getVisibleVertices()) {
            final Rect r = graphView.getVertexShape(a);
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

    protected void HAMToShapes(final GraphView2D g) {
        for (final Atom a : g.getVisibleVertices()) {
            final Vector v = ham.getCoordinates().get(a);
            
            Rect tr = g.getVertexShape(a);
            if (v.getDimensions() == 2) {
                float x = (float) v.getCoordinate(1) * hamScale;
                float y = (float) v.getCoordinate(2) * hamScale;
                g.setTargetCenter(tr, x, y, 0);
            } else if (v.getDimensions() == 3) {
                float x = (float) v.getCoordinate(1) * hamScale;
                float y = (float) v.getCoordinate(2) * hamScale;
                float z = (float) v.getCoordinate(3) * hamScale;
                g.setTargetCenter(tr, x, y, z);
            }
        }        
    }
    
    @Override
    protected void update(final GraphView2D g) {
        for (int i = 0; i < alignCycles; i++) {
            ham.align();
        }
        HAMToShapes(g);
    }

    @Override
    public boolean isReady(GraphView2D g) {
        return accumulated > g.param.getLayoutUpdatePeriod();
    }
    
}
