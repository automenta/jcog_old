package jcog.critterding;

import java.util.LinkedList;
import java.util.List;

/**
 * see archneuronz.h
 */
public class NeuronBuilder {

    // inhibitory neuron by flag
    boolean isInhibitory;
    // Consistent Synapses flag
    boolean hasConsistentSynapses;
    // inhibitory synapses flag
    boolean hasInhibitorySynapses;
    // neuron firing potential
    double firingThreshold;
    
    // motor neuron ability (excititatory only) flag
    //boolean isMotor; //isMotor if motor!=null
    // function
    MotorNeuron motor;
    // synaptic plasticity by flag
    boolean isPlastic;
    // factors
    double plasticityStrengthen;
    double plasticityWeaken;
    List<SynapseBuilder> synapseBuilders = new LinkedList();

    public InterNeuron newNeuron(int maxSynapses) {
        InterNeuron ni = new InterNeuron();

        ni.maxSynapses = maxSynapses;

        ni.isInhibitory = isInhibitory;
        ni.firingThreshold = firingThreshold;

        ni.motor = motor;

        ni.isPlastic = isPlastic;
        ni.plasticityStrengthen = 1.0f + (1.0f / plasticityStrengthen);
        ni.plasticityWeaken = 1.0f - (1.0f / plasticityWeaken);
        ni.synapseBuilders = synapseBuilders;


        return ni;
    }
}
