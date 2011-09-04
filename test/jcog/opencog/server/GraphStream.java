/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.server;

import jcog.opencog.OCMind;
import org.restlet.Context;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;
import org.restlet.Server;
import org.restlet.data.Protocol;
import org.restlet.resource.Finder;
import org.restlet.resource.ServerResource;

/**
 *
 * @author seh
 */
public class GraphStream {
    private final OCMind mind;
    private GraphStreamSession session;

    public GraphStream(final OCMind mind) throws Exception {
        this.mind = mind;
        
        /*
        public Server(Protocol protocol, int port, Class<?> nextClass) {
           this((Context) null, protocol, port, new Finder(Context.getCurrent(), nextClass));
        }
        */
        Finder f = new Finder(Context.getCurrent(), GraphStreamResource.class) {
            
            @Override public ServerResource create(Class<? extends ServerResource> targetClass, Request request, Response response) {
                ServerResource sr = super.create(targetClass, request, response);
                
                if (sr instanceof GraphStreamResource) {
                    GraphStreamResource gsr = (GraphStreamResource)sr;
                    session = gsr.setMind(mind);
                }
                
                return sr;
            }
            
        };

        Server s = new Server(Protocol.HTTP, 8182, f);           
                
        s.start();
    }
    
}
