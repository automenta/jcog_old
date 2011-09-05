package jcog.critterding;

import com.syncleus.dann.neural.SimpleSynapse;
import java.util.Collection;
import java.util.List;

public class InterNeuron extends MotorNeuron {

    double output, nextOutput;
    int maxSynapses;
    boolean isInhibitory;
    double firingThreshold;
    MotorNeuron motor;
    //List<Synapse> synapses = new LinkedList();
    double potential;
    double potentialDecay;
    boolean isPlastic;
    double plasticityStrengthen;
    double plasticityWeaken;
    double maxWeight = 1.0f;
    List<SynapseBuilder> synapseBuilders;

    public InterNeuron() {
        super();
        // 	maxSynapses			= 150;
        isInhibitory = false;
        // input weight range, 5 = -5 <-> +5	-> 10 in total because 0 will be excluded
// 	dendridicBranches		= 10;
        // processing
// 	firingThreshold			= 30.0f;
        potential = 0.0F;
        potentialDecay = 0.95F;
        // output
        output = 0;
        nextOutput = 0;
        // plasticity up & down
        isPlastic = false;
// 	plasticityStrengthen		= 1.0f+(1.0f/100.0f);
// 	plasticityWeaken		= 1.0f-(1.0f/1000.0f);
        // SPECIALO optional reference that makes this a MOTOR neuron, but it depends on not being defined
        motor = null;
    }

    public void forward(final Collection<SimpleSynapse<CritterdingNeuron>> synapses) {        

        // potential decay
        potential *= potentialDecay;

        // make every connection do it's influence on the neuron's total potential
        
        for (SimpleSynapse<CritterdingNeuron> s : synapses) {
            // lower synaptic weights
            if (isPlastic) {
                s.setWeight(s.getWeight() * plasticityStrengthen);
            }

            potential += s.getWeight() * s.getInput();
        }
        
//        if ((potential!=0) || (output!=0))
//            System.out.println(this + " pot=" + potential + ", out=" + output + " " + isInhibitory + " " + firingThreshold);

        if (isInhibitory) {
            forwardInhibitory(synapses);
        } else {
            forwardExhibitory(synapses);
        }
        
    }

    protected void forwardInhibitory(final Collection<SimpleSynapse<CritterdingNeuron>> synapses) {
        // do we spike/fire
        if (potential <= -1.0f * firingThreshold) {
            // reset neural potential
            potential = 0.0f;

            // fire the neuron
            nextOutput = -1;

            // PLASTICITY: if neuron & synapse fire together, the synapse strenghtens
            if (isPlastic) {
                for (SimpleSynapse<CritterdingNeuron> s : synapses) {
                    double o = s.getInput();
                    // if synapse fired, strenghten the weight
                    final double w = s.getWeight();
                    if ((o < 0.0f && w > 0.0f) || (o > 0.0f && w < 0.0f)) {
                        // 						cerr << endl << "Inhibitory firing" << endl << "synref: " << *Synapses[i].ref << endl << "pre weight:  " << Synapses[i].weight << endl;
                        s.setWeight(w * plasticityStrengthen);
                        // 						cerr << "post weight: " << Synapses[i].weight << endl;
                    }

                    // clamp weight
                    clampWeight(s);
                }
            }
        } // don't fire the neuron
        else {
            nextOutput = 0;
            // reset potential if < 0
            if (potential > 0.0f) {
                potential = 0.0f;
            }
        }
    }

    protected void forwardExhibitory(final Collection<SimpleSynapse<CritterdingNeuron>> synapses) {
        // do we spike/fire
        if (potential >= firingThreshold) {
            // reset neural potential
            potential = 0.0f;

            // fire the neuron
            nextOutput = 1;

            // PLASTICITY: if neuron & synapse fire together, the synapse strenghtens
            if (isPlastic) {
                for (SimpleSynapse<CritterdingNeuron> s : synapses) {
                    double o = s.getInput();

                    final double w = s.getWeight();

                    // if synapse fired, strenghten the weight
                    if ((o > 0.0f && w > 0.0f) || (o < 0.0f && w < 0.0f)) {
                        // 						cerr << endl << "Excititory firing" << endl << "synref: " << *Synapses[i].ref << endl << "pre weight:  " << Synapses[i].weight << endl;
                        s.setWeight(w * plasticityStrengthen);
                        // 						cerr << "post weight: " << Synapses[i].weight << endl;
                        }

                    // if weight > max back to max
                    clampWeight(s);
                }
            }
        } // don't fire the neuron
        else {
            nextOutput = 0;
            // reset potential if < 0
            if (potential < 0.0f) {
                potential = 0.0f;
            }
        }
    }

    private void clampWeight(final SimpleSynapse<CritterdingNeuron> s) {
        s.setWeight(Math.max(Math.min(s.getWeight(), maxWeight), -maxWeight));
    }


    public double getPotential() {
        return potential;
    }

    @Override
    public double getOutput() {
        return output;
    }


}
