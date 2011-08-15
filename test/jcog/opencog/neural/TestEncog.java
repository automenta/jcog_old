/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.neural;

import java.util.HashMap;
import java.util.Map;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import jcog.opencog.swing.AttentionControlPanel;
import jcog.opencog.swing.GraphPanel;
import jcog.opencog.swing.GraphView;
import jcog.spacegraph.swing.SwingWindow;
import org.encog.engine.network.activation.ActivationFunction;
import org.encog.engine.network.activation.ActivationSigmoid;
import org.encog.ml.BasicML;
import org.encog.ml.data.MLDataSet;
import org.encog.ml.data.basic.BasicMLDataSet;
import org.encog.ml.train.MLTrain;
import org.encog.ml.train.strategy.Greedy;
import org.encog.ml.train.strategy.HybridStrategy;
import org.encog.ml.train.strategy.StopTrainingStrategy;
import org.encog.neural.networks.BasicNetwork;
import org.encog.neural.networks.training.CalculateScore;
import org.encog.neural.networks.training.TrainingSetScore;
import org.encog.neural.networks.training.anneal.NeuralSimulatedAnnealing;
import org.encog.neural.networks.training.propagation.back.Backpropagation;
import org.encog.neural.pattern.ElmanPattern;
import org.encog.neural.pattern.FeedForwardPattern;

/**
 *
 * @author seh
 */
public class TestEncog {

    public static class EncogAgent extends MindAgent {

        private final BasicML network;
        private final MLTrain train;
        
        
        Map<String, Atom> neurons = new HashMap();  //holds neurons and edges

        public EncogAgent(double period, BasicML network, MLTrain train) {
            super(period);
            this.network = network;
            this.train = train;
        }

        public String getAtomID(int layer, int neuron) {
            return layer + "x" + neuron;
        }
        public Atom getAtom(final OCMind mind, int layer, int neuron) {
            String atomId = getAtomID(layer, neuron);

            Atom a = neurons.get(atomId);
            if (a == null) {
                a = mind.addVertex(AtomType.conceptNode, atomId);
                neurons.put(atomId, a);
            }

            return a;
        }
        
        public Atom getEdge(final OCMind mind, int fromLayer, int fromNeuron, int toLayer, int toNeuron) {
            String edgeID = getAtomID(fromLayer, fromNeuron) + "-" + getAtomID(toLayer, toNeuron);
            Atom e = neurons.get(edgeID);
            if (e == null) {
                Atom a = getAtom(mind, fromLayer, fromNeuron);
                Atom b = getAtom(mind, toLayer, toNeuron);                
                e = mind.addEdge(AtomType.orderedLink, edgeID, a, b);
                neurons.put(edgeID, e);
            }
            return e;
            
        }
        

        @Override
        protected void run(final OCMind mind) {
            ActivationFunction act;
            if (network instanceof BasicNetwork) {
                //atomize layers
                BasicNetwork bn = (BasicNetwork) network;

                for (int i = 0; i < bn.getLayerCount(); i++) {
                    int n = bn.getLayerTotalNeuronCount(i);

                    for (int x = 0; x < n; x++) {
                        Atom a = getAtom(mind, i, x);
                        double o = bn.getLayerOutput(i, x);
                        setStimulus(a, (short) (o * 100));
                        

                        if (i < bn.getLayerCount()-1) {
                            for (int y = 0; y < bn.getLayerTotalNeuronCount(i+1); y++) {
                                try {
                                    double w = bn.getWeight(i, x, y);
                                    Atom e = getEdge(mind, i, x, i+1, y);
                                    mind.getTruth(e).setMean(w);
                                }
                                catch (ArrayIndexOutOfBoundsException ex) {
                                    
                                }
//                                if (bn.isConnected(i, x, y)) {
//                                    System.out.println("connected: " + i + " " + x + " " + y);
//                                }
//                                else {
//                                    System.out.println("NOT connected: " + i + " " + x + " " + y);                                    
//                                }
                            }
                        }
                    }

                }


            }
            
            train.iteration();
            System.out.println(train.getError());
        }
    }

    public static class TemporalXOR {

        /**
         * 1 xor 0 = 1, 0 xor 0 = 0, 0 xor 1 = 1, 1 xor 1 = 0
         */
        public static final double[] SEQUENCE = {1.0, 0.0, 1.0, 0.0, 0.0, 0.0,
            0.0, 1.0, 1.0, 1.0, 1.0, 0.0};
        private double[][] input;
        private double[][] ideal;

        public MLDataSet generate(final int count) {
            this.input = new double[count][1];
            this.ideal = new double[count][1];

            for (int i = 0; i < this.input.length; i++) {
                this.input[i][0] = TemporalXOR.SEQUENCE[i
                        % TemporalXOR.SEQUENCE.length];
                this.ideal[i][0] = TemporalXOR.SEQUENCE[(i + 1)
                        % TemporalXOR.SEQUENCE.length];
            }

            return new BasicMLDataSet(this.input, this.ideal);
        }
    }

    static BasicNetwork createElmanNetwork() {
        // construct an Elman type network
        ElmanPattern pattern = new ElmanPattern();
        pattern.setActivationFunction(new ActivationSigmoid());
        pattern.setInputNeurons(1);
        pattern.addHiddenLayer(16);
        pattern.setOutputNeurons(1);
        return (BasicNetwork) pattern.generate();
    }

    static BasicNetwork createFeedforwardNetwork() {
        // construct a feedforward type network
        FeedForwardPattern pattern = new FeedForwardPattern();
        pattern.setActivationFunction(new ActivationSigmoid());
        pattern.setInputNeurons(1);
        pattern.addHiddenLayer(4);
        pattern.setOutputNeurons(1);
        return (BasicNetwork) pattern.generate();
    }

    public static void main(final String args[]) {

        final TemporalXOR temp = new TemporalXOR();
        final MLDataSet trainingSet = temp.generate(120);

        final BasicNetwork network = createElmanNetwork();
        //final BasicNetwork network = createFeedforwardNetwork();

        //final double elmanError = trainNetwork("Elman", elmanNetwork,               trainingSet);
//        final double feedforwardError = ElmanXOR.trainNetwork("Feedforward",
//                feedforwardNetwork, trainingSet);

        final CalculateScore score = new TrainingSetScore(trainingSet);
        final MLTrain trainMain = new Backpropagation(network, trainingSet, 0.000001, 0.0);
        final MLTrain trainAlt = new NeuralSimulatedAnnealing(network, score, 10, 2, 100);

        final StopTrainingStrategy stop = new StopTrainingStrategy();
        trainMain.addStrategy(new Greedy());
        trainMain.addStrategy(new HybridStrategy(trainAlt));
        //trainMain.addStrategy(stop);

//        int epoch = 0;
//        while (!stop.shouldStop()) {
//            trainMain.iteration();
////            System.out.println("Training " + what + ", Epoch #" + epoch
////                    + " Error:" + trainMain.getError());
//            epoch++;
//        }
//        System.out.println("Best error rate with Elman Network: " + elmanError);
////        System.out.println("Best error rate with Feedforward Network: "
////                + feedforwardError);
//        System.out.println("Elman should be able to get into the 10% range,\nfeedforward should not go below 25%.\nThe recurrent Elment net can learn better in this case.");
//        System.out.println("If your results are not as good, try rerunning, or perhaps training longer.");


        OCMind mind = new OCMind();

        mind.addAgent(new EncogAgent(0.5, network, trainMain));

        new AttentionControlPanel(mind, 0.75).newWindow();          
        new SwingWindow(new GraphPanel(new GraphView(mind)), 800, 800, true);

        mind.start(0.04);

        //Encog.getInstance().shutdown();
    }

    public static double trainNetwork(final String what, final BasicNetwork network, final MLDataSet trainingSet) {
        // train the neural network
        CalculateScore score = new TrainingSetScore(trainingSet);
        final MLTrain trainAlt = new NeuralSimulatedAnnealing(network, score, 10, 2, 100);

        final MLTrain trainMain = new Backpropagation(network, trainingSet, 0.000001, 0.0);

        final StopTrainingStrategy stop = new StopTrainingStrategy();
        trainMain.addStrategy(new Greedy());
        trainMain.addStrategy(new HybridStrategy(trainAlt));
        trainMain.addStrategy(stop);

        int epoch = 0;
        while (!stop.shouldStop()) {
            trainMain.iteration();
            System.out.println("Training " + what + ", Epoch #" + epoch
                    + " Error:" + trainMain.getError());
            epoch++;
        }
        return trainMain.getError();
    }
}
//public static void main(String args[]) {
//        // create the training set
//        MLDataSet training = new BasicMLDataSet(SOM_INPUT, null);
//
//        // Create the neural network.
//        SOM network = new SOM(4, 2);
//        network.reset();
//
//        BasicTrainSOM train = new BasicTrainSOM(
//                network,
//                0.7,
//                training,
//                new NeighborhoodSingle());
//
//        int iteration = 0;
//
//        for (iteration = 0; iteration <= 10; iteration++) {
//            train.iteration();
//            System.out.println("Iteration: " + iteration + ", Error:" + train.getError());
//        }
//
//        MLData data1 = new BasicMLData(SOM_INPUT[0]);
//        MLData data2 = new BasicMLData(SOM_INPUT[1]);
//        System.out.println("Pattern 1 winner: " + network.winner(data1));
//        System.out.println("Pattern 2 winner: " + network.winner(data2));
//        Encog.getInstance().shutdown();
//    }

