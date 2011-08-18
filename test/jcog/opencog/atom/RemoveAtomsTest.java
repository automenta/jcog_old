/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.atom;

import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.OCMind;
import junit.framework.TestCase;

/**
 *
 * @author seh
 */
public class RemoveAtomsTest extends TestCase {
    
    
    public void testRemoveVertex() {
        OCMind m = new OCMind();
        
        Atom a = m.addVertex(AtomType.conceptNode, "A");
        
        //do removal
        assertTrue(m.remove(a));
        
        assertTrue(m.getVertices().isEmpty());
    }

    public void testRemoveEdge() {
        OCMind m = new OCMind();
        
        Atom a = m.addVertex(AtomType.conceptNode, "A");
        Atom b = m.addVertex(AtomType.conceptNode, "B");
        Atom c = m.addEdge(AtomType.link, a, b);
        
        assertTrue(m.getEdges().size() == 1);
        
        //do removal
        assertTrue(m.remove(c));
        
        assertTrue(m.getVertices().size() == 2);
        assertTrue(m.getEdges().isEmpty());
    }

    public void testRemoveIncidentVertex() {
        OCMind m = new OCMind();
        
        Atom a = m.addVertex(AtomType.conceptNode, "A");
        Atom b = m.addVertex(AtomType.conceptNode, "B");
        Atom c = m.addEdge(AtomType.link, a, b);
        
        //do removal; should cause c to be removed
        assertTrue(m.remove(b));
        
        assertTrue(m.getVertices().size() == 1);
        assertTrue(m.getEdges().isEmpty());
    }
    
}
