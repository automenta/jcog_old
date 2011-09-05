package jcog.critterding;

import com.syncleus.dann.neural.OutputNeuron;

public class MotorNeuron implements CritterdingNeuron, OutputNeuron {
    boolean firing;

    @Override
    public double getOutput() {
        return firing ? 1.0 : 0.0;
    }

    @Override
    public void tick() {
    }


}
