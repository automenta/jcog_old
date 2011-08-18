/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog.util;

import com.google.common.collect.Multimap;
import java.io.PrintStream;
import java.util.Collection;
import jcog.opencog.Atom;
import jcog.opencog.atom.MemoryAtomSpace;
import jcog.opencog.AtomType;
import jcog.opencog.OCMind;

/**
 *
 * @author seh
 */
public class AtomSpacePrinter {
    
    /** TODO generalize this to ReadableAtomSpace */
    public void print(MemoryAtomSpace a, PrintStream o) {
        o.println("Vertices: (" + a.getVertices().size() + ") " + a.getVertices());
        o.println("Edges: ("  + a.getEdges().size() + ") " + a.getEdges());

//        o.println("Types:");
//        Multimap<Class<? extends AtomType>, Atom> typeIndex = a.getTypeIndex();
//        for (Class<? extends AtomType> t : typeIndex.keySet()) {
//            Collection<Atom> types = typeIndex.get(t);
//            o.println("  " + t.getSimpleName() + " (" + types.size() + "): " + types);            
//        }
        
//        o.println("Names: " + a.getNameIndex());
        o.println("Mean Edge Arity: " + a.getMeanEdgeArity());
    }
}
