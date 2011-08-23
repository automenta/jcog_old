/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.OCMind;
import jcog.opencog.util.AtomizeJSON;
import org.codehaus.jackson.JsonNode;

/**
 * @see http://wiki.gephi.org/index.php/Specification_-_GSoC_Graph_Streaming_API
 * @author seh
 */
public class GraphStreamInput {
    private final OCMind mind;
    private final String namePrefix;

    Map<String, Atom> vertexNames = new HashMap();
    
    public enum GraphStreamOpcode {
        an, cn, dn, ae, ce, de, other
    }
    
    public class GSAtomizeJSON extends AtomizeJSON {

        
        public GSAtomizeJSON(String url) {
            super(url);
        }
        
        @Override
        public void nextObject(JsonNode n) {
            GraphStreamOpcode g = GraphStreamOpcode.other;
            
            if (n.has("an"))
                g = g.an;
            else if (n.has("cn"))
                g = g.cn;                
            else if (n.has("dn"))
                g = g.dn;                
            else if (n.has("ae"))
                g = g.ae;                
            else if (n.has("de"))
                g = g.de;                
            else if (n.has("ce"))
                g = g.ce;                
            
            //TODO support batch operations
            
            if (g!= g.other) {
                JsonNode an = n.get(g.toString());
                Iterator<String> ii = an.getFieldNames();
                while (ii.hasNext()) {
                    String fn = ii.next();
                    if (g == g.an)
                        addNode(an, fn);
                    else if (g == g.ae)
                        addEdge(an, fn);
                    
                }
            }
            
        }


    }

    public GraphStreamInput(OCMind mind, String namePrefix, String url) {
        super();
        this.mind = mind;
        this.namePrefix = namePrefix;
        
        new GSAtomizeJSON(url);
    }
    
    public static String getParameter(JsonNode j, String field, String defaultValue) {
        if (j.has(field)) {
            return j.get(field).getValueAsText();
        }
        return defaultValue;
    }
    
    protected void addNode(JsonNode an, String id) {
        JsonNode xn = an.get(id);
        String label = getParameter(xn, "label", id);
                
        Atom v = mind.addVertex(AtomType.conceptNode, namePrefix + id);
        
        vertexNames.put(id, v);        
    }
    
    
    public Atom getVertex(String v) {
        return vertexNames.get(v);
    }
    
    protected void addEdge(JsonNode an, String id) {
        JsonNode xn = an.get(id);
        String label = getParameter(xn, "label", id);
        String source = getParameter(xn, "source", null);
        String target = getParameter(xn, "target", null);
        boolean directed = getParameter(xn, "directed", "false").equals("true");
        
        final Class<? extends AtomType> edgeType = directed ? AtomType.orderedLink : AtomType.unorderedLink;
        mind.addEdge(edgeType, namePrefix + label, getVertex(source), getVertex(target));
    }
    
    
    public static void main(String[] args) {
        new GraphStreamInput(new OCMind(), "amazon_", "http://www.di.unito.it/~panisson/amazon.json");
    }
}
