/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.critterding;

import java.util.HashMap;
import java.util.Map;
import jcog.critterding.Synapse.MotorSynapse;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.GraphSpace;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import jcog.opencog.attention.DecaySTI;

/**
 *
 * @author seh
 */
public class DemoCritterdingNeuron {

    public static class AsyncNeuronAgent extends MindAgent {
        private final CritterdingBrain brain;
        boolean needsRefresh;
        
        Map<AbstractNeuron, Atom> neuronAtom = new HashMap();
        Map<Synapse, Atom> synapseEdge = new HashMap();

        public AsyncNeuronAgent(CritterdingBrain b, double period) {
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
//            for (InterNeuron in : brain.getNeuron()) {
//                for (Synapse s : in.getSynapses()) {
//                    mind.addEdge(AtomType.orderedLink, neuronAtom.get(s.inputNeuron), neuronAtom.get(in));
//                }  
//                if (in.motor!=null) {
//                    mind.addEdge(AtomType.orderedLink, neuronAtom.get(in), neuronAtom.get(in.motor));
//                }
//            }
            synapseEdge.clear();
            for (Synapse s : brain.getEdges()) {
                Atom e = mind.addEdge(AtomType.orderedLink, neuronAtom.get(s.getSourceNode()), neuronAtom.get(s.getDestinationNode()));
                synapseEdge.put(s, e);
            }

        }
        
        @Override
        protected void run(OCMind mind) {
            if (needsRefresh) {
                refresh(mind);
                needsRefresh = false;
            }

            
            double momentum = 0.5;
            for (SenseNeuron sn : brain.getSense()) {
                sn.senseInput = sn.senseInput * momentum + (1.0 - momentum) * RandomNumber.getDouble(-1.0, 1.0) * 0.5;
            }

            brain.forward();
            
            for (AbstractNeuron an : neuronAtom.keySet()) {
                Atom a = neuronAtom.get(an);
                
                double scaleFactor = 50.0;
                addStimulus(a, (short)(an.getOutput()*scaleFactor));
            }
            
            for (Synapse s : synapseEdge.keySet()) {
                Atom e = synapseEdge.get(s);
                if (!(s instanceof MotorSynapse)) {
                    mind.getTruth(e).setMean(0.5 * s.weight);
                }
                else
                    mind.getTruth(e).setMean(1.0);
            }
            
        }
        
    }
    
    public DemoCritterdingNeuron() {
        super();
        
        int inputs = 8;
        int outputs = 16;
        int numNeurons = 64;
        int minSynapsesPerNeuron = 1;
        int maxSynapsesPerNeuron = 3;
        
        CritterdingBrain b = new BrainBuilder(inputs, outputs).newBrain(numNeurons, minSynapsesPerNeuron, maxSynapsesPerNeuron);
        System.out.println(b.getNodes().size());
        System.out.println(b.getEdges().size());
        
        OCMind m = new OCMind();
        m.addAgent(new AsyncNeuronAgent(b, 0));
        m.addAgent(new DecaySTI(0, (short)50));
        
        new GraphSpace(m);
        m.run(0.01);
        
//        m.cycle();
//        try {
//            new GraphStreamOutput(m, "/tmp/x.json");
//        } catch (Exception ex) {
//            ex.printStackTrace();
//        }
    }
    
    
    public static void main(String[] args) {
        new DemoCritterdingNeuron();
    }
}
