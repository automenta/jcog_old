/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import jcog.opencog.swing.GraphView2D;

/**
 *
 * @author seh
 */
public abstract class GraphViewProcess  {
    protected double accumulated = 0;
    protected OCMind mind;
    //protected GraphView2D graphView;
    

    public GraphViewProcess() {
        super();
    }

    
    public void _update(GraphView2D g) {
//        if (this.graphView == null) {
//            this.graphView = g;
            this.mind = g.getMind();
//        }

        accumulated = 0;
        update(g);
    }

    abstract public void reset(GraphView2D g);

    protected abstract void update(GraphView2D g);

    public abstract boolean isReady(GraphView2D g);

    public void accumulate(double dt) {
        accumulated += dt;
    }

    public double getAccumulated() {
        return accumulated;
    }
    
}
