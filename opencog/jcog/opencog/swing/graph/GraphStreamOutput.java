/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.OCMind;
import jcog.opencog.util.AtomizeXML;

/**
 * Generates JSON GraphStreaming API support for visualization by Gephi
 * @see http://wiki.gephi.org/index.php/Specification_-_GSoC_Graph_Streaming_API
 * @author seh
 */
public class GraphStreamOutput {
    private final OCMind mind;

    public GraphStreamOutput(OCMind mind, String path) throws FileNotFoundException, IOException {
        this(mind, new FileOutputStream(new File(path)));
    }

    public static class AddNode {
        public AddNode(Atom a, String label) {
            
        }
    }
    
    public static String getAddNodeString(String id, String label) {
        StringBuffer sb = new StringBuffer();      
                
        int size = 1;
        
        String p = "";
        if (label!=null) {
            label = label.trim();
            p += "\"label\": \"" + label + "\", \"size\":" + size; 
        }
        
        final String x = "\"" + id + "\":{" + p + "}";
        sb.append("{\"an\":{" + x + "}}");
        
        return sb.toString();
    }
    public static String getAddEdgeString(OCMind mind, Atom edge, Atom source, Atom target) {
        StringBuffer sb = new StringBuffer();
        final String id = "e_" + edge.uuid.toString();
           
        int size = 1;
        
        String label = mind.getName(edge);
        final String edgeName = mind.getTypeName(edge);
        
        if (label == null)
            label = edgeName;

        boolean directed = AtomType.orderedLink.isAssignableFrom( mind.getType(edge) );
        
        String p = "";
        if (label!=null) {       
            label = label.trim();
            p += "\"label\": \"" + label + "\", \"size\":" + size + ", \"edgeType\":" + edgeName + ", \"directed\":" + (directed ? "true": "false") + ","; 
        }
        p += "\"source\":" + "\"" + source +"\"," + "\"target\":" + "\"" + target +"\"";
        
        final String x = "\"" + id + "\":{" + p + "}";
        sb.append("{\"ae\":{" + x + "}}");
        
        return sb.toString();
    }
    
    public GraphStreamOutput(OCMind mind, FileOutputStream out) throws IOException {
        super();
        
        this.mind = mind;
        
        final MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment> graph = mind.foldHypergraphEdges();
        System.out.println(graph);
        System.out.println(graph.getNodes().size());
        System.out.println(graph.getEdges().size());
        
//        JsonFactory jsonFactory = new JsonFactory(new ObjectMapper()); // or, for data binding, org.codehaus.jackson.mapper.MappingJsonFactory 
//        JsonGenerator g = jsonFactory.createJsonGenerator(out);
        
        PrintStream bw = new PrintStream(out);
        for (Atom a : graph.getNodes()) {
            bw.println(getAddNodeString(a.uuid.toString(), mind.getName(a)) + "\r");
        }
        for (HyperedgeSegment a : graph.getEdges()) {
            bw.println(getAddEdgeString(mind, a.parentEdge, a.getSourceNode(), a.getDestinationNode()) + "\r");            
        }

        out.close();
  
    }
    
    
    
    
    public static void main(String[] args) {
        OCMind mind = new OCMind();
        new AtomizeXML(mind, "/tmp/x.xml");
        
        try {
            new GraphStreamOutput(mind, "/tmp/x.json");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
