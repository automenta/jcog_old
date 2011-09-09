/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.atom.dann;

import com.syncleus.dann.graph.HyperEdge;
import com.syncleus.dann.graph.xml.EdgeXml;
import com.syncleus.dann.xml.Namer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import jcog.opencog.Atom;

/**
 *
 * @author seh
 */
public class AtomEdge extends Atom implements HyperEdge<Atom> {
    private final List<Atom> nodes;

    public AtomEdge(Atom e, List<Atom> nodes) {
        this.nodes = nodes;
    }

    @Override
    public List<Atom> getNodes() {
        return Collections.unmodifiableList(nodes);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AtomEdge) {
            AtomEdge ae = (AtomEdge) obj;
            return (uuid.equals(ae.uuid)) && (ae.getNodes().equals(getNodes()));
        }
        return false;
    }

    @Override
    public int hashCode() {
        return super.hashCode() + getNodes().hashCode();
    }

    @Override
    public int getDegree() {
        return this.getNodes().size();
    }

    @Override
    public boolean isSymmetric(HyperEdge he) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public HyperEdge<Atom> connect(Atom n) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public HyperEdge<Atom> connect(List<Atom> list) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public HyperEdge<Atom> disconnect(Atom n) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public HyperEdge<Atom> disconnect(List<Atom> list) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public HyperEdge<Atom> clone() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public List<Atom> getTraversableNodes(Atom node) {
        //FROM: AbstractHyperEdge
        final List<Atom> traversableNodes = new ArrayList(this.getNodes());
        if (!traversableNodes.remove(node)) {
            throw new IllegalArgumentException("node is not one of the end points!");
        }
        return Collections.unmodifiableList(traversableNodes);
    }

    @Override
    public boolean isTraversable(Atom n) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public EdgeXml toXml() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public EdgeXml toXml(Namer<Object> namer) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void toXml(EdgeXml t, Namer<Object> namer) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public boolean isContextEnabled() {
        return false;
    }
    
}
