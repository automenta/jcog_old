package jcog.critterding;

import com.syncleus.dann.graph.AbstractDirectedEdge;

public class Synapse  {

    
    public final AbstractNeuron inputNeuron;   // InputNeuron's Output Pointer
    
    double dendriteBranch;   // dendridic branch synapse is located on
    
    double weight;   // it's synaptic weight -1.0f <-> +1.0f

    public Synapse(AbstractNeuron inputNeuron, double dendriteBranch, float synapticWeight) {
        super();
        this.inputNeuron = inputNeuron;
        this.dendriteBranch = dendriteBranch;
        this.weight = synapticWeight;
    }

    public double getInput() {
        return inputNeuron.getOutput();
    }

}
