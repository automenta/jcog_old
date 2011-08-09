/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.hopfield;

import jcog.opencog.attention.Forget;
import jcog.opencog.attention.LearnHebbian;
import jcog.opencog.attention.SpreadImportance;
import jcog.opencog.swing.GraphView;
import jcog.opencog.swing.graph.GraphViewProcess;

/**
 *
 * See: http://wiki.opencog.org/w/Hopfield_network_emulator
 * @author seh
 */
public class HopfieldExample2 extends HopfieldExample {
    
    public HopfieldExample2(int width, int height, double density) {
        super(width, height);

        AtomArray2D bitmap = new AtomArray2D(this, getClass().getSimpleName(), width, height);
        //wireRandomly(bitmap, density);
        wireMesh(bitmap);

        /* A number of HebbianLinks are also randomly distributed
         * to connect these nodes, (the number is specified either
         * directly or by the density parameter). 
         */
        //TODO detect if density is too high, making it impossible to wire

//        if (density > 0) {
//            wireRandomly(bitmap, density);
//        }
//        else {
//            wireMesh(bitmap);
//        }

        
        
//        addAgent(imprint);       
//        
//        addAgent(new UpdateImportance());        
//        addAgent(new LearnHebbian());
//        addAgent(new SpreadImportance());
//        addAgent(new Forget());
        
        //new AtomSpacePrinter().print(atomspace, System.out);
        
        //AtomArray2DPanel bitmapPanel = new AtomArray2DPanel(bitmap, this);
        //bitmapPanel.newWindow();
        
        //new AgentControlPanel(this).newWindow();
        
        
        final ImprintBitmap imprint = new ImprintBitmap(bitmap, (short)32, 1.0);
        imprint.setRandom();       
        imprint.setPeriod(0.5);
        addAgent(imprint);
        
        LearnHebbian lh = new LearnHebbian();
        lh.setPeriod(0.15);
        addAgent(lh);
        
        SpreadImportance si = new SpreadImportance();
        si.setPeriod(0.05);
        addAgent(si);
        
        //addAgent(new Forget());

        GraphViewProcess p1 = new GraphViewProcess() {

            @Override
            protected void update(GraphView g) {
                cycle();
            }

            @Override
            public boolean isReady() {
                return getAccumulated() > 0.03;
            }
            
        };
                
        GraphView.newGraphWindow(this, p1);        
    }

    public static void main(String[] args) {
        new HopfieldExample2(4, 4, 1.6);
    }
}
