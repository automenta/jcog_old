/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.xml;

import jcog.opencog.MindRunner;
import jcog.opencog.util.AtomizeXML;
import jcog.opencog.OCMind;
import jcog.opencog.attention.LearnHebbian;
import jcog.opencog.attention.RandomStimulation;
import jcog.opencog.attention.SpreadImportance;
import jcog.opencog.swing.AttentionControlPanel;
import jcog.opencog.swing.GraphPanel;
import jcog.opencog.swing.GraphView;
import jcog.spacegraph.swing.SwingWindow;

/**
 *
 * @author seh
 */
public class TestAtomizeXML {

    public static void main(String[] args) {
        OCMind m = new OCMind();
        new AtomizeXML("/tmp/x.xml", m);
        
        m.printAtoms();
        
        LearnHebbian lh = new LearnHebbian();
        m.addAgent(lh);
        
        final SpreadImportance si = new SpreadImportance();
        m.addAgent(si);
//        
        m.addAgent(new RandomStimulation(1.0, (short)100, 1));
        
        
        new AttentionControlPanel(m, 0.5).newWindow();          
        new SwingWindow(new GraphPanel(new GraphView(m)), 800, 800, true);

        m.start(0.1);

    }
}
