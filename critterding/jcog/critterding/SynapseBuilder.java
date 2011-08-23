package jcog.critterding;

/**
 * see archsynapse.h
 */
public class SynapseBuilder {
    // determines if id referes to an interneuron or sensorneuron
    boolean isSensorNeuron;
    // id of neuron which axon connects to this synapse

    //InterNeuron neuron;

    //int realneuronID;
    int neurontargetlayer;
    // dendridic weight according
    int dendriteBranches;
    // its "weight"
    float weight;
    SenseNeuron senseNeuron;

    public SynapseBuilder() {
        super();
        isSensorNeuron = false;
        neurontargetlayer = 0;
        dendriteBranches = 1;
        weight = 0.0F;
    }

}
