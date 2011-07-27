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
        final int numVertices = 500;
        final int numEdges = 2000;
        final int targetVertices = numVertices/2;
        final int targetEdges = numEdges/2;
        
        OCMind m = new OCMind();

        printMemory("Before Random Graph Generation");
        
        new RandomGraphGenerator(m, numVertices, numEdges);
                
        assertEquals(m.getVertexCount(), numVertices);
        assertEquals(m.getEdgeCount(), numEdges);
        
        printMemory("After Random Graph Generation");
        
        m.cycle();
        
        RandomStimulation rs = new RandomStimulation(0, (short)0, numVertices*10) {
            @Override
            public short getBoost() {
                return (short)RandomNumber.getInt(0, 127);
            }            
        };
        m.addAgent(rs);
        
        m.cycle();
        m.cycle();
        
        printGraphSize(m);
        
        Forget f = new Forget(0, targetVertices, targetEdges) {

            @Override
            public void run(OCMind mind) {
                super.run(mind);
                System.out.println("Forgetting atoms.. vert=" + mind.getVertexCount() + ", edge=" + mind.getEdgeCount());
                System.out.println(mind.attention.size() + " " + IteratorUtils.toList(mind.iterateAtomsByIncreasingSTI()).size() + " " + mind.getAtoms().size());
            }
            
        };
        m.addAgent(f);
        
        int cyclesToFinish = 0;
        while ((targetVertices < m.getVertexCount()) || (targetEdges < m.getEdgeCount())) {
            m.cycle();
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
