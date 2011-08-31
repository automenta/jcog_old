/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.server;

import org.restlet.representation.Representation;
import org.restlet.resource.Get;
import org.restlet.resource.ServerResource;

/**
 *
 * @author seh
 */
public class GraphStreamResource extends ServerResource {

    @Get
    public Representation toJSON() {
        return new GraphStreamSession(TestGraphStream.mind).representation;
    }
    //        @Get
    //        public String toString() {
    //            return "hello, world";
    //        }
    
}
