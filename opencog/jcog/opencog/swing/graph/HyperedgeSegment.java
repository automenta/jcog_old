/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import com.syncleus.dann.graph.AbstractDirectedEdge;
import jcog.opencog.Atom;

/**
 *
 * @author seh
 */
public class HyperedgeSegment extends AbstractDirectedEdge<Atom> {
    //TODO add support for multiple parallel edges from src to dest
    
    public final String label;
    public final Atom parentEdge;

    public HyperedgeSegment(final Atom src, final Atom dest, final Atom parentEdge, final String label) {
        super(src, dest);
        this.label = label;
        this.parentEdge = parentEdge;
    }

    @Override
    public String toString() {
        return label;        
    }
    
    @Override
    public int hashCode() {
        return parentEdge.hashCode() + getSourceNode().hashCode() + getDestinationNode().hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof HyperedgeSegment) {
            HyperedgeSegment fe = (HyperedgeSegment) obj;
            return (fe.parentEdge == parentEdge) && (fe.getSourceNode() == getSourceNode()) && (fe.getDestinationNode() == getDestinationNode());
        }
        return false;
    }
    
}
