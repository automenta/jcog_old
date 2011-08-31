/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.server;

import jcog.opencog.OCMind;
import org.restlet.Server;
import org.restlet.data.Protocol;

/**
 *
 * @author seh
 */
public class GraphStream {
    private final OCMind mind;

    public GraphStream(OCMind mind) throws Exception {
        this.mind = mind;
        Server s = new Server(Protocol.HTTP, 8182, GraphStreamResource.class);
        s.start();
    }
    
}
