/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.util;

import java.io.EOFException;
import java.io.IOException;
import java.net.URL;
import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonNode;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.map.ObjectMapper;

/**
 *
 * @author seh
 */
public abstract class AtomizeJSON {
    private JsonParser jp;

    public AtomizeJSON(final String url) {
        try {
            
            JsonFactory jsonFactory = new JsonFactory(new ObjectMapper()); // or, for data binding, org.codehaus.jackson.mapper.MappingJsonFactory 
            jp = jsonFactory.createJsonParser(new URL(url)); // or URL, Stream, Reader, String, byte[]
            
            boolean finished = false;
            while (!finished) {
                try {
                    readNext();
                }
                catch (EOFException e) {
                    finished = true;
                }
                catch (IOException f) {
                    f.printStackTrace();
                    finished = true;
                }
            }
            
//            InputStream is = new URL(url).openStream();
//
//            //System.out.println(sb);
//             ObjectMapper mapper = new ObjectMapper();
//            
//             while (is.available() > 0) {
//                // (note: can also use more specific type, like ArrayNode or ObjectNode!)
//                JsonNode rootNode = mapper.readValue(is, JsonNode.class); // src can be a File, URL, InputStream etc
//                System.out.println(rootNode);
//                System.out.println(is.available());
//             }
//            
//            is.close();
            
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    public void readNext() throws IOException, EOFException {
        JsonNode tn = jp.readValueAsTree();
        nextObject(tn);
    }
    
    abstract public void nextObject(JsonNode n);
    
    public static void main(String[] args) {
        new AtomizeJSON("http://www.di.unito.it/~panisson/amazon.json") {

            @Override
            public void nextObject(JsonNode n) {
                System.out.println(n);
            }
            
        };
    }
}

