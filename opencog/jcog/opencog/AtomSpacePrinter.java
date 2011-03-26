/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog;

import com.google.common.collect.Multimap;
import java.io.PrintStream;
import java.util.Collection;

/**
 *
 * @author seh
 */
public class AtomSpacePrinter {

    /** TODO generalize this to ReadableAtomSpace */
    public void print(MemoryAtomSpace a, PrintStream o) {
        o.println("Vertices: (" + a.getVertices().size() + ") " + a.getVertices());
        o.println("Edges: ("  + a.getEdges().size() + ") " + a.getEdges());

        o.println("Types:");
        Multimap<OCType, Atom> typeIndex = a.getTypeIndex();
        for (OCType t : typeIndex.keySet()) {
            Collection<Atom> types = typeIndex.get(t);
            o.println("  " + t + " (" + types.size() + "): " + types);            
        }
        
        o.println("Names: " + a.getNameIndex());
        o.println("Mean Edge Arity: " + a.getMeanEdgeArity());
    }
}
