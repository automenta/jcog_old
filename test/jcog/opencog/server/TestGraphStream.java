/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.server;

import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import jcog.opencog.Atom;
import jcog.opencog.OCMind;
import jcog.opencog.hopfield.HopfieldExample;
import jcog.opencog.swing.graph.HyperedgeSegment;
import org.apache.commons.collections15.IteratorUtils;
import org.restlet.Server;
import org.restlet.data.Protocol;
import org.restlet.representation.Representation;
import org.restlet.resource.Get;
import org.restlet.resource.ServerResource;

/**
 *
 * @author seh
 */
public class TestGraphStream {

    public static OCMind mind;

    public static void main(String[] args) {
        try {
            int inputs = 32;
            int outputs = 32;
            int numNeurons = 128;
            int minSynapsesPerNeuron = 1;
            int maxSynapsesPerNeuron = 4;


            //mind = new OCMind();

//            Brain b = new BrainBuilder(inputs, outputs).newBrain(numNeurons, minSynapsesPerNeuron, maxSynapsesPerNeuron);
//            System.out.println(b.getNodes().size());
//            System.out.println(b.getEdges().size());
//
//            mind.addAgent(new AsyncNeuronAgent(b, 0));


            mind = new HopfieldExample(8, 8, 0);

            mind.cycle();

            new GraphStream(mind);

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
