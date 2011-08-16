/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import jcog.opencog.Atom;
import jcog.opencog.OCMind;
import jcog.opencog.swing.GraphView2D;
import jcog.spacegraph.shape.Rect;
import jcog.spacegraph.shape.TrapezoidLine;

//    @Deprecated
/**
 *
 * @author me
 */
public interface GraphView2DRenderer {

    public Rect newVertex(OCMind mind, Atom v);

    public void updateVertex(GraphView2D gv, Atom vertex, Rect r);

    public TrapezoidLine newEdge(OCMind mind, Atom e, Atom source, Atom target, Rect sourceRect, Rect targetRect);

    public void updateEdge(GraphView2D aThis, Atom parentEdge, TrapezoidLine get);
    
}
