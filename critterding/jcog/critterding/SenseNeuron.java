package jcog.critterding;

public class SenseNeuron implements AbstractNeuron {
    
    public double senseInput;

    @Override
    public double getOutput() {
        return senseInput;
    }


    
}
