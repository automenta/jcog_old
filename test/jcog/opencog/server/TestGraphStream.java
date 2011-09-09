/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.server;

import jcog.graphstream.GraphStream;
import jcog.critterding.CritterdingBrain;
import jcog.critterding.DemoCritterdingNeuron.AsyncNeuronAgent;
import jcog.opencog.OCMind;
import jcog.opencog.attention.AddRandomHebbianEdges;
import jcog.opencog.attention.LearnHebbian;

/**
 *
 * @author seh
 */
public class TestGraphStream {

    public static void main(String[] args) {
        try {
            int inputs = 64;
            int outputs = 16;
            int numNeurons = 256;
            int minSynapsesPerNeuron = 2;
            int maxSynapsesPerNeuron = 6;


            OCMind mind = new OCMind();

            CritterdingBrain b = new CritterdingBrain(inputs, outputs, numNeurons, minSynapsesPerNeuron, maxSynapsesPerNeuron);
            System.out.println(b.getNodes().size());
            System.out.println(b.getEdges().size());

            mind.addAgent(new AsyncNeuronAgent(b, 0));
            mind.addAgent(new LearnHebbian());
            mind.addAgent(new AddRandomHebbianEdges(0.5, 3, 2, 16, 32));


            //mind = new HopfieldExample(8, 8, 0);
            
//            new TestCoreNLP(mind);

            new GraphStream(mind, 8182);

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
