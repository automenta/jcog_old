/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import jcog.opencog.OCMind;
import jcog.opencog.swing.GraphView;

/**
 *
 * @author seh
 */
public abstract class GraphViewProcess {
    protected double accumulated = 0;
    protected final OCMind mind;
    protected final GraphView graphView;

    public GraphViewProcess(GraphView gv) {
        super();
        this.graphView = gv;
        this.mind = gv.getMind();
    }

    public void _update(GraphView g) {
        accumulated = 0;
        update(g);
    }

    public void reset() {
    }

    protected abstract void update(GraphView g);

    public abstract boolean isReady();

    public void accumulate(double dt) {
        accumulated += dt;
    }

    public double getAccumulated() {
        return accumulated;
    }
    
}
