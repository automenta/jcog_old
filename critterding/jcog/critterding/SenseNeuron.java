package jcog.critterding;

public class SenseNeuron implements AbstractNeuron {
    
    public double senseInput;

    public void setInput(double senseInput) {
        this.senseInput = senseInput;
    }
    
    @Override
    public double getOutput() {
        return senseInput;
    }


    
}
