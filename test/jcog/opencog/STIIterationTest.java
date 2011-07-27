/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog;

import java.util.List;
import jcog.math.RandomNumber;
import jcog.opencog.attention.RandomStimulation;
import jcog.opencog.util.RandomGraphGenerator;
import junit.framework.TestCase;
import org.apache.commons.collections15.IteratorUtils;

/**
 *
 * @author seh
 */
public class STIIterationTest extends TestCase {
    
    public void testSTIIteration() {
        
        int numVertices = 16;
        int numEdges = 8;
        
        OCMind m = new OCMind();
        
        new RandomGraphGenerator(m, numVertices, numEdges);

        RandomStimulation rs = new RandomStimulation(0, (short)0, numVertices*100) {
            @Override
            public short getBoost() {
                return (short)RandomNumber.getInt(0, 127);
            }            
        };
        m.addAgent(rs);
        
        m.cycle();
        m.cycle();        
        
        assertEquals(m.getVertexCount(), numVertices);
        assertEquals(m.getEdgeCount(), numEdges);
        assertTrue(m.getMinSeenSTI()!=m.getMaxSeenSTI());
        
        int numAtoms = IteratorUtils.toList(m.iterateAtoms()).size();
        assertEquals(numAtoms, m.getVertexCount() + m.getEdgeCount());
        assertEquals(numAtoms, IteratorUtils.toList(m.iterateAtoms()).size());
        
        assertEquals(numAtoms, IteratorUtils.toList(m.iterateAtomsByIncreasingSTI()).size());
        
                
//        List<Atom> dec = IteratorUtils.toList(m.iterateAtomsByIncreasingSTI());
//        System.out.println("Decreasing: " + dec.size() + " " + dec);
//        for (Atom a : dec) {
//            System.out.println("  " + a + " "+ m.getSTI(a));
//        }
//        
//        System.out.println("All atoms: " + m.getAtoms().size());
//        for (Atom a : m.getAtoms()) {
//            System.out.println("  " + a + " "+ m.getSTI(a));            
//        }
    }
}
