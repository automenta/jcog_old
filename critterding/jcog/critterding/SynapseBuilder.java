package jcog.critterding;

/**
 * see archsynapse.h
 */
public class SynapseBuilder {
    // determines if id referes to an interneuron or sensorneuron
    final public boolean isSensorNeuron;
    
    final public float weight;

    public SynapseBuilder(final float weight, final boolean isSensorNeuron) {
        super();
        this.isSensorNeuron = isSensorNeuron;
        this.weight = weight;
    }

}
