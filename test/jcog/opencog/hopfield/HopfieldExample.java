/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.hopfield;

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

        public ImprintBitmap(AtomArray2D array) {
            super("HopfieldExampleImprintBitmap");
            
            this.array = array;
        }
        
        public void setRandom(short min, short max) {
            for (Atom a : array.atoms) {
                setStimulus(a, (short)RandomNumber.getInt(min, max));                
            }
        }
        
        @Override
        public void run(OCMind mind) {
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

        ImprintBitmap imprint = new ImprintBitmap(bitmap);
        imprint.setRandom((short)0, (short)100);       
        addAgent(imprint);       
        
        UpdateImportance updateImportance = new UpdateImportance();
        updateImportance.run(this); //TODO remove this hack when OCMind runs its own agents
        addAgent(updateImportance);
        
        addAgent(new LearnHebbian());
        addAgent(new SpreadImportance());
        addAgent(new Forget());
        
        new AtomSpacePrinter().print(atomspace, System.out);
        
        AtomArray2DPanel bitmapPanel = new AtomArray2DPanel(bitmap, this);
        bitmapPanel.newWindow();

    }

    public static void main(String[] args) {
        new HopfieldExample(4, 4, 2.0);
    }
}
