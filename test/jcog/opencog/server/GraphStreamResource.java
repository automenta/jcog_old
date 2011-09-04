/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.server;

import jcog.opencog.OCMind;
import org.restlet.representation.Representation;
import org.restlet.resource.Get;
import org.restlet.resource.ServerResource;

/**
 *
 * @author seh
 */
public class GraphStreamResource extends ServerResource {
    private OCMind mind;
    private GraphStreamSession session;

    @Get
    public Representation toJSON() {
        session.start();
        return session.representation;
    }
    //        @Get
    //        public String toString() {
    //            return "hello, world";
    //        }

    public GraphStreamSession setMind(OCMind mind) {
        this.mind = mind;
        this.session = new GraphStreamSession(this.mind);
        return session;
    }
    
}
