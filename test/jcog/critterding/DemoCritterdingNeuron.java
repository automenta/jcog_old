/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.critterding;

import com.syncleus.dann.neural.AbstractLocalBrain;
import com.syncleus.dann.neural.InputNeuron;
import com.syncleus.dann.neural.OutputNeuron;
import com.syncleus.dann.neural.SimpleSynapse;
import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import javax.swing.JPanel;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import jcog.opencog.attention.DecaySTI;
import jcog.spacegraph.swing.SwingWindow;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.LineAndShapeRenderer;
import org.jfree.data.category.DefaultCategoryDataset;

/**
 *
 * @author seh
 */
public class DemoCritterdingNeuron {

    public static class AsyncNeuronAgent extends MindAgent {

        private final CritterdingBrain brain;
        boolean needsRefresh;
        Map<CritterdingNeuron, Atom> neuronAtom = new HashMap();
        Map<SimpleSynapse<CritterdingNeuron>, Atom> synapseEdge = new HashMap();

        public AsyncNeuronAgent(CritterdingBrain b, double period) {
            super(period);
            this.brain = b;
            needsRefresh = true;
        }

        protected void refresh(OCMind mind) {
            //TODO remove existing atoms?

            neuronAtom.clear();

            int senseN = 0;
            for (SenseNeuron in : brain.getSense()) {
                Atom a = mind.addVertex(AtomType.conceptNode, "SenseNeuron." + senseN);
                neuronAtom.put(in, a);
                senseN++;
            }

            int motorN = 0;
            for (MotorNeuron in : brain.getMotor()) {
                Atom a = mind.addVertex(AtomType.conceptNode, "MotorNeuron." + motorN);
                neuronAtom.put(in, a);
                motorN++;
            }

            int inN = 0;
            for (InterNeuron in : brain.getNeuron()) {
                //TODO use special type
                Atom a = mind.addVertex(AtomType.conceptNode, "InterNeuron." + inN);
                neuronAtom.put(in, a);
                inN++;

            }

            //add interneuron connections
//            for (InterNeuron in : brain.getNeuron()) {
//                for (Synapse s : in.getSynapses()) {
//                    mind.addEdge(AtomType.orderedLink, neuronAtom.get(s.inputNeuron), neuronAtom.get(in));
//                }  
//                if (in.motor!=null) {
//                    mind.addEdge(AtomType.orderedLink, neuronAtom.get(in), neuronAtom.get(in.motor));
//                }
//            }
            synapseEdge.clear();
            for (SimpleSynapse<CritterdingNeuron> s : brain.getEdges()) {
                Atom e = mind.addEdge(AtomType.orderedLink, neuronAtom.get(s.getSourceNode()), neuronAtom.get(s.getDestinationNode()));
                synapseEdge.put(s, e);
            }

        }

        @Override
        protected void run(OCMind mind) {
            if (needsRefresh) {
                refresh(mind);
                needsRefresh = false;
            }


            double minInputValue = -1.0;
            double minOutputValue = 1.0;
            
            double momentum = 0.5;
            for (SenseNeuron sn : brain.getSense()) {
                sn.senseInput = sn.senseInput * momentum + (1.0 - momentum) * RandomNumber.getDouble(minInputValue, minOutputValue) * 0.5;
            }

            brain.forward();

            for (CritterdingNeuron an : neuronAtom.keySet()) {
                Atom a = neuronAtom.get(an);

                double scaleFactor = 50.0;
                addStimulus(a, (short) (an.getOutput() * scaleFactor));
            }

            for (SimpleSynapse<CritterdingNeuron> s : synapseEdge.keySet()) {
                Atom e = synapseEdge.get(s);
                mind.getTruth(e).setMean(0.5 * s.getWeight());
//                if (!(s instanceof MotorSynapse)) {
//                }
//                else
//                    mind.getTruth(e).setMean(1.0);
            }

        }
    }

    public abstract static class BrainGraphPanel extends JPanel {

        final DefaultCategoryDataset dataset = new DefaultCategoryDataset();
        private final JFreeChart chart;
        protected final AbstractLocalBrain brain;

        public BrainGraphPanel(AbstractLocalBrain b) {
            super();
            this.brain = b;
            
            //setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
            setLayout(new BorderLayout());
            
            // create the chart...

            chart = ChartFactory.createLineChart(
                    "", // chart title
                    "Time", // domain axis label
                    "Output", // range axis label
                    dataset, // data
                    PlotOrientation.VERTICAL, // orientation
                    true, // include legend
                    true, // tooltips
                    false // urls
                    );

            // NOW DO SOME OPTIONAL CUSTOMISATION OF THE CHART...
//        final StandardLegend legend = (StandardLegend) chart.getLegend();
            //      legend.setDisplaySeriesShapes(true);
            //    legend.setShapeScaleX(1.5);
            //  legend.setShapeScaleY(1.5);
            //legend.setDisplaySeriesLines(true);

            chart.setBackgroundPaint(Color.white);

            final CategoryPlot plot = (CategoryPlot) chart.getPlot();
            plot.setBackgroundPaint(Color.lightGray);
            plot.setRangeGridlinePaint(Color.white);

            // customise the range axis...
            final NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
            rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
            rangeAxis.setAutoRangeIncludesZero(true);

            // ****************************************************************************
            // * JFREECHART DEVELOPER GUIDE                                               *
            // * The JFreeChart Developer Guide, written by David Gilbert, is available   *
            // * to purchase from Object Refinery Limited:                                *
            // *                                                                          *
            // * http://www.object-refinery.com/jfreechart/guide.html                     *
            // *                                                                          *
            // * Sales are used to provide funding for the JFreeChart project - please    * 
            // * support us so that we can continue developing free software.             *
            // ****************************************************************************

            // customise the renderer...
            final LineAndShapeRenderer renderer = (LineAndShapeRenderer) plot.getRenderer();
//        renderer.setDrawShapes(true);

            renderer.setSeriesStroke(
                    0, new BasicStroke(
                    2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
                    1.0f, new float[]{10.0f, 6.0f}, 0.0f));
            renderer.setSeriesStroke(
                    1, new BasicStroke(
                    2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
                    1.0f, new float[]{6.0f, 6.0f}, 0.0f));
            renderer.setSeriesStroke(
                    2, new BasicStroke(
                    2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
                    1.0f, new float[]{2.0f, 6.0f}, 0.0f));
            // OPTIONAL CUSTOMISATION COMPLETED.

            add(new ChartPanel(chart), BorderLayout.CENTER);
        }


        abstract public void refresh();
    }

    public static class BrainInputsGraphPanel extends BrainGraphPanel {

        Integer x = 0;
        
        public BrainInputsGraphPanel(AbstractLocalBrain b) {
            super(b);
        }

        @Override
        public void refresh() {
            final Set<InputNeuron> in = brain.getInputNeurons();
            for (final InputNeuron i : in)
                dataset.addValue(i.getInput(), i.toString(), x );
            
            x++;
        }        
        
    }

    public static class BrainOutputsGraphPanel extends BrainGraphPanel {

        Integer x = 0;
        
        public BrainOutputsGraphPanel(AbstractLocalBrain b) {
            super(b);
        }

        @Override
        public void refresh() {
            final Set<OutputNeuron> in = brain.getOutputNeurons();
            for (final OutputNeuron i : in)
                dataset.addValue(i.getOutput(), i.toString(), x );
            
            x++;
        }        
        
    }

    public DemoCritterdingNeuron() {
        super();

        int inputs = 4;
        int outputs = 4;
        int numNeurons = 128;
        int minSynapsesPerNeuron = 3;
        int maxSynapsesPerNeuron = 5;

        CritterdingBrain b = new CritterdingBrain(inputs, outputs, numNeurons, minSynapsesPerNeuron, maxSynapsesPerNeuron);
        System.out.println(b.getNodes().size());
        System.out.println(b.getEdges().size());

        OCMind m = new OCMind();
        m.addAgent(new AsyncNeuronAgent(b, 0.6));
        //m.addAgent(new DecaySTI(0, (short) 50));

        
        final BrainInputsGraphPanel bi = new BrainInputsGraphPanel(b);
        final BrainOutputsGraphPanel bo = new BrainOutputsGraphPanel(b);
        new SwingWindow(bi, 400, 600);
        new SwingWindow(bo, 400, 600);
        
        m.addAgent(new MindAgent(0.3) {

            @Override
            protected void run(OCMind mind) {
                bi.refresh();
                bo.refresh();
            }
            
        });

        //new GraphSpace(m);
        m.run(0.01);

//        m.cycle();
//        try {
//            new GraphStreamOutput(m, "/tmp/x.json");
//        } catch (Exception ex) {
//            ex.printStackTrace();
//        }
    }

    public static void main(String[] args) {
        new DemoCritterdingNeuron();
    }
}
