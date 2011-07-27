/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog;

import jcog.math.RandomNumber;
import jcog.opencog.attention.Forget;
import jcog.opencog.attention.RandomStimulation;
import jcog.opencog.util.RandomGraphGenerator;
import junit.framework.TestCase;
import org.apache.commons.collections15.IteratorUtils;

/**
 *
 * @author seh
 */
public class ForgetTest extends TestCase {
    
    
    public void testForget() {
        final int numVertices = 1500;
        final int numEdges = 3000;
        final int targetVertices = numVertices/2;
        final int targetEdges = numEdges/2;
        
        OCMind m = new OCMind();

        printMemory("Before Random Graph Generation");
        
        new RandomGraphGenerator(m, numVertices, numEdges);
                
        assertEquals(m.getVertexCount(), numVertices);
        assertEquals(m.getEdgeCount(), numEdges);
        
        printMemory("After Random Graph Generation");
        
        m.cycle();
        
        RandomStimulation rs = new RandomStimulation(0, (short)0, numVertices*100) {
            @Override
            public short getBoost() {
                return (short)RandomNumber.getInt(0, 127);
            }            
        };
        m.addAgent(rs);
        
        m.cycle();
        m.cycle();
        
        printGraphSize(m);
        
        Forget f = new Forget(0, targetVertices, targetEdges);
        m.addAgent(f);
        
        int cyclesToFinish = 0;
        while ((targetVertices < m.getVertexCount()) || (targetEdges < m.getEdgeCount())) {
            m.cycle();
            System.out.println("Forgetting atoms.. vert=" + m.getVertexCount() + ", edge=" + m.getEdgeCount());
            System.out.println(" " + IteratorUtils.toList(m.iterateAtomsByDecreasingSTI()).size() + " " + IteratorUtils.toList(m.iterateAtoms()).size());
            cyclesToFinish++;
        }
        
        System.out.println("Forget complete in " + cyclesToFinish + " cycles");
        printGraphSize(m);
    }
    
    public static void printMemory(String label) {
        System.gc();
        long used = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
        System.out.println(label + ": bytes used = " + used);
    }

    public void printGraphSize(OCMind m) {
        System.out.println("Verts= " + m.getVertexCount() + ", Edges=" + m.getEdgeCount());
    }
    
    public static void main(String[] args) {
        new ForgetTest().testForget();
    }
}
