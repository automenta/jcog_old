/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.server;

import edu.stanford.nlp.ling.Document;
import java.io.InputStream;
import org.restlet.Server;
import org.restlet.data.Protocol;
import org.restlet.representation.Representation;
import org.restlet.resource.Delete;
import org.restlet.resource.Get;
import org.restlet.resource.Post;
import org.restlet.resource.Put;
import org.restlet.resource.ServerResource;

/**
 *
 * @author seh
 */
public class TestServer {

    public static class TestResource extends ServerResource {
        @Get  
        public String toString() {  
            return "hello, world";  
        }
  
//
//        @Get("xml")
//        public Representation toXml() {
//            // ...
//            return null;
//        }
//
//        @Post("xml")
//        public Representation accept(Document entity) {
//            // ...
//            return null;
//        }
//
//        @Put("cust")
//        public void storeXml(InputStream stream) {
//            // ...
//        }
//
//        @Delete
//        public void removeAll() {
//            // ...
//        }
    }

    public static void main(String[] args) {
        try {
            new Server(Protocol.HTTP, 8182, TestResource.class).start();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
