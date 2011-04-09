/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.hopfield;

import java.util.HashMap;
import java.util.Map;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomSpacePrinter;
import jcog.opencog.AtomTypes;
import jcog.opencog.OCMind;
import jcog.opencog.DefaultOCMind;
import jcog.opencog.MindAgent;
import jcog.opencog.attention.Forget;
import jcog.opencog.attention.LearnHebbian;
import jcog.opencog.attention.SpreadImportance;
import jcog.opencog.attention.UpdateImportance;

/**
 *
 * See: http://wiki.opencog.org/w/Hopfield_network_emulator
 * @author seh
 */
public class HopfieldExample extends DefaultOCMind {

    public static class ImprintBitmap extends MindAgent {
        private final AtomArray2D array;

        Map<Atom, Double> pattern = new HashMap();
        private final short maxPixelStimulus;
        private final double imprintProbability;
        private boolean imprinting;
        
        public ImprintBitmap(AtomArray2D array, short maxPixelStimulus, double imprintProbabaility) {
            super("HopfieldExampleImprintBitmap");
            
            this.array = array;
            this.maxPixelStimulus = maxPixelStimulus;
            this.imprintProbability = imprintProbabaility;
        }
        
        public void setPixel(Atom a, double value) {
            pattern.put(a, value);
        }
        
        public double getPixel(Atom a) {
            Double d = pattern.get(a);
            if (d == null)
                return 0;
            return d.doubleValue();
        }
        
        public short getStimulus(Atom a) {
            if (imprinting) {
                double v = getPixel(a);
                return (short)(v * maxPixelStimulus);
            }
            else {
                return 0;
            }
        }
        
        public void setZero() {
            for (Atom a : array.atoms) {
                setPixel(a, 0);
            }            
        }
        
        public void setRandom() {
            setRandom(0, 1.0);
        }
        
        public void setRandom(double min, double max) {
            for (Atom a : array.atoms) {
                setPixel(a, RandomNumber.getDouble(min, max));
            }
        }
        
        void imprint() {            
            imprinting = true;
            for (Atom a : array.atoms) {
                setStimulus(a, getStimulus(a));
            }            
        }
        
        void blink() {
            imprinting = false;
        }
        
        @Override
        public void run(OCMind mind) {
            if (Math.random() < imprintProbability)
                imprint();
            else
                blink();
        }
        
    }

    public HopfieldExample(int width, int height, double density) {
        super();

        final int numNodes = width * height;

        AtomArray2D bitmap = new AtomArray2D(HopfieldExample.this, getClass().getSimpleName(), width, height);

        /* A number of HebbianLinks are also randomly distributed
         * to connect these nodes, (the number is specified either
         * directly or by the density parameter). 
         */
        //TODO detect if density is too high, making it impossible to wire

        int numLinks = (int) Math.floor(numNodes * density);

        while (numLinks > 0) {

            Atom sourceNode = bitmap.getRandomNode();
            Atom targetNode = bitmap.getRandomNode();
            
            if (sourceNode == targetNode) {
                continue;
            }

            if (getEdge(AtomTypes.SymmetricHebbianLink, sourceNode, targetNode) == null) {
                addEdge(AtomTypes.SymmetricHebbianLink, sourceNode, targetNode);
                numLinks--;
            }
            
        }

        ImprintBitmap imprint = new ImprintBitmap(bitmap, (short)32, 1.0);
        imprint.setRandom(0, 1.0);       
        addAgent(imprint);       
        
        addAgent(new UpdateImportance());        
        addAgent(new LearnHebbian());
        addAgent(new SpreadImportance());
        addAgent(new Forget());
        
        //new AtomSpacePrinter().print(atomspace, System.out);
        
        AtomArray2DPanel bitmapPanel = new AtomArray2DPanel(bitmap, this);
        bitmapPanel.newWindow();
        
        new AgentControlPanel(this).newWindow();
        
        GraphView1.newGraphWindow(this);        

    }

    public static void main(String[] args) {
        new HopfieldExample(8, 8, 4.0);
    }
}
