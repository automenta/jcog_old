package jcog.critterding;

import java.util.LinkedList;
import java.util.List;

public class InterNeuron extends MotorNeuron {

    double output, nextOutput;
    int maxSynapses;
    boolean isInhibitory;
    double firingThreshold;
    double dendridicBranches;
    MotorNeuron motor;
    List<Synapse> synapses = new LinkedList();
    double potential;
    double potentialDecay;
    boolean isPlastic;
    double plasticityStrengthen;
    double plasticityWeaken;
    double maxWeight = 5.0f;
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

    public void forward() {        

        // potential decay
        potential *= potentialDecay;

        // make every connection do it's influence on the neuron's total potential
        
        for (Synapse s : synapses) {
            // lower synaptic weights
            if (isPlastic) {
                s.weight *= plasticityWeaken;
            }

            potential += s.weight * s.dendriteBranch * s.getInput();
        }
        
//        if ((potential!=0) || (output!=0))
//            System.out.println(this + " pot=" + potential + ", out=" + output + " " + isInhibitory + " " + firingThreshold);

        if (isInhibitory) {
            forwardInhibitory();
        } else {
            forwardExhibitory();
        }
        
    }

    /**
    [18:53] <bobke> so this function is called at wireArch
    [18:55] <bobke> first argument is a pointer to the output of the neuron he synapse will connect to
    [18:55] <bobke> (the phenotype neuron)
    [18:55] <bobke> on which branch and with what weight
    [18:56] <bobke> it'll create a synapse in the neuroninter
    [18:56] <bobke> which has as an input argument 1
     */
    public void newSynapse(AbstractNeuron incoming, double dendriteBranch, float synapticWeight) {
        Synapse s = new Synapse(incoming, dendriteBranch, synapticWeight);
        synapses.add(s);
    }

    protected void forwardInhibitory() {
        // do we spike/fire
        if (potential <= -1.0f * firingThreshold) {
            // reset neural potential
            potential = 0.0f;

            // fire the neuron
            nextOutput = -1;

            // PLASTICITY: if neuron & synapse fire together, the synapse strenghtens
            if (isPlastic) {
                for (Synapse s : synapses) {
                    double o = s.getInput();
                    // if synapse fired, strenghten the weight
                    if ((o < 0.0f && s.weight > 0.0f) || (o > 0.0f && s.weight < 0.0f)) {
                        // 						cerr << endl << "Inhibitory firing" << endl << "synref: " << *Synapses[i].ref << endl << "pre weight:  " << Synapses[i].weight << endl;
                        s.weight *= plasticityStrengthen;
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

    protected void forwardExhibitory() {
        // do we spike/fire
        if (potential >= firingThreshold) {
            // reset neural potential
            potential = 0.0f;

            // fire the neuron
            nextOutput = 1;

            // PLASTICITY: if neuron & synapse fire together, the synapse strenghtens
            if (isPlastic) {
                for (Synapse s : synapses) {
                    double o = s.getInput();

                    // if synapse fired, strenghten the weight
                    if ((o > 0.0f && s.weight > 0.0f) || (o < 0.0f && s.weight < 0.0f)) {
                        // 						cerr << endl << "Excititory firing" << endl << "synref: " << *Synapses[i].ref << endl << "pre weight:  " << Synapses[i].weight << endl;
                        s.weight *= plasticityStrengthen;
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

    private void clampWeight(Synapse s) {
        s.weight = Math.min(s.weight, maxWeight);
        s.weight = Math.max(s.weight, -maxWeight);
    }


    public double getPotential() {
        return potential;
    }

    @Override
    public double getOutput() {
        return output;
    }

    public List<Synapse> getSynapses() {
        return synapses;
    }




}
