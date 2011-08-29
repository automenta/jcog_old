package jcog.critterding;

import com.syncleus.dann.graph.AbstractDirectedEdge;

public class Synapse extends AbstractDirectedEdge<AbstractNeuron> {
    
    public final AbstractNeuron inputNeuron;   // InputNeuron's Output Pointer
    
    double weight;   // it's synaptic weight -1.0f <-> +1.0f

    public Synapse(AbstractNeuron inputNeuron, AbstractNeuron outputNeuron, float synapticWeight) {
        super(inputNeuron, outputNeuron);
        this.inputNeuron = inputNeuron;
        this.weight = synapticWeight;
    }

    public double getInput() {
        return inputNeuron.getOutput();
    }
    
    public static class MotorSynapse extends Synapse {

        public MotorSynapse(AbstractNeuron inputNeuron, MotorNeuron outputNeuron) {
            super(inputNeuron, outputNeuron, 0);
        }
        
    }

}
