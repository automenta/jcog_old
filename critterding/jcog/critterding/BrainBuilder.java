/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.critterding;

/**
 *
 * @author seh
 */
public class BrainBuilder {
    private final int inputs;
    private final int outputs;

    public BrainBuilder(int inputs, int outputs) {
        super();
        this.inputs = inputs;
        this.outputs = outputs;
    }

    public Brain newBrain(int numNeurons, int minSynapsesPerNeuron, int maxSynapsesPerNeuron) {
        Brain b = new Brain(numNeurons, minSynapsesPerNeuron, maxSynapsesPerNeuron);

        for (int i = 0; i < inputs; i++)
            b.newInput();

        for (int i = 0; i < outputs; i++)
            b.newOutput();

        //TODO move buildArch and all parameters it uses to this, out of Brain
        b.buildArch();
        b.wireArch();

        return b;
    }

}
