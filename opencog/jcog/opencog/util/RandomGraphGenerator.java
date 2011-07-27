/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.util;

import java.util.ArrayList;
import java.util.List;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.OCMind;

/**
 *
 * @author seh
 */
public class RandomGraphGenerator {
    
    //TODO add parameter for # of vertices in each created edge
    
    public RandomGraphGenerator(OCMind mind, int numVertices, int numEdges) {
        super();
        
        List<Atom> vAdded = new ArrayList(numVertices);
        while (numVertices > 0) {
            Atom a = mind.addVertex(AtomType.conceptNode, "v" + numVertices);
            vAdded.add(a);
            numVertices--;
        }
        
        while (numEdges > 0) {
            int x = RandomNumber.getInt(0, vAdded.size()-1);
            int y;
            do {
                y = RandomNumber.getInt(0, vAdded.size()-1);
            } while (y == x);
            
            Atom ax = vAdded.get(x);
            Atom ay = vAdded.get(y);
            if (mind.getEdge(AtomType.orderedLink, ax, ay)==null) {
                mind.addEdge(AtomType.orderedLink, vAdded.get(x), vAdded.get(y));
                numEdges--;
            }
        }
        
    }
}
