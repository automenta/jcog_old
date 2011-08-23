/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.critterding;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import jcog.opencog.util.GraphStreamOutput;

/**
 *
 * @author seh
 */
public class DemoCritterdingNeuron {

    public static class AsyncNeuronAgent extends MindAgent {
        private final Brain brain;
        boolean needsRefresh;
        
        Map<AbstractNeuron, Atom> neuronAtom = new HashMap();

        public AsyncNeuronAgent(Brain b, double period) {
            super(period);
            this.brain = b;
            needsRefresh = true;
        }

        
        protected void refresh(OCMind mind) {
            //TODO remove existing atoms?
            
            neuronAtom.clear();
            
            int senseN = 0;
            for (SenseNeuron in : brain.getSense()) {
                Atom a = mind.addVertex(AtomType.conceptNode, "SenseNeuron." + senseN);
                neuronAtom.put(in, a);
                senseN++;
            }
            
            int motorN = 0;
            for (MotorNeuron in : brain.getMotor()) {
                Atom a = mind.addVertex(AtomType.conceptNode, "MotorNeuron." + motorN);
                neuronAtom.put(in, a);
                motorN++;
            }
            
            int inN = 0;
            for (InterNeuron in : brain.getNeuron()) {
                //TODO use special type
                Atom a = mind.addVertex(AtomType.conceptNode, "InterNeuron." + inN);
                neuronAtom.put(in, a);
                inN++;
                
            }
            
            //add interneuron connections
            for (InterNeuron in : brain.getNeuron()) {
                for (Synapse s : in.getSynapses()) {
                    mind.addEdge(AtomType.orderedLink, neuronAtom.get(s.inputNeuron), neuronAtom.get(in));
                }  
                if (in.motor!=null) {
                    mind.addEdge(AtomType.orderedLink, neuronAtom.get(in), neuronAtom.get(in.motor));
                }
            }

        }
        
        @Override
        protected void run(OCMind mind) {
            if (needsRefresh) {
                refresh(mind);
                needsRefresh = false;
            }
            for (SenseNeuron sn : brain.getSense()) {
                sn.senseInput+= RandomNumber.getDouble(-1.0, 1.0) * 0.3;
            }
            
            for (AbstractNeuron an : neuronAtom.keySet()) {
                Atom a = neuronAtom.get(an);
                
                double scaleFactor = 10.0;
                addStimulus(a, (short)(an.getOutput()*scaleFactor));
            }
            
            brain.forward();
        }
        
    }
    
    public DemoCritterdingNeuron() {
        super();
        
        int inputs = 16;
        int outputs = 16;
        int numNeurons = 256;
        int minSynapsesPerNeuron = 1;
        int maxSynapsesPerNeuron = 8;
        
        Brain b = new BrainBuilder(inputs, outputs).newBrain(numNeurons, minSynapsesPerNeuron, maxSynapsesPerNeuron);
        BrainGraph bg = new BrainGraph(b);
        System.out.println(bg.getNodes());
        System.out.println(bg.getEdges());
        
        OCMind m = new OCMind();
        m.addAgent(new AsyncNeuronAgent(b, 0));
        
//        new GraphSpace(m);
        //m.run(0.01);
        
        m.cycle();
        try {
            new GraphStreamOutput(m, "/tmp/x.json");
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    
    public static void main(String[] args) {
        new DemoCritterdingNeuron();
    }
}
