/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.xml;

import java.awt.Color;
import jcog.opencog.OCMind;
import jcog.opencog.attention.AddRandomHebbianEdges;
import jcog.opencog.attention.DecaySTI;
import jcog.opencog.attention.Forget;
import jcog.opencog.attention.LearnHebbian;
import jcog.opencog.attention.MessageTokenizer;
import jcog.opencog.attention.RandomStimulation;
import jcog.opencog.attention.SpreadImportance;
import jcog.opencog.swing.AttentionControlPanel;
import jcog.opencog.swing.ConsoleWindow.JavascriptConsoleWindow;
import jcog.opencog.swing.GraphPanel;
import jcog.opencog.swing.GraphView2D;
import jcog.opencog.swing.graph.GraphStreamInput;
import jcog.spacegraph.swing.SwingWindow;

/**
 *
 * @author seh
 */
public class TestAtomizeXML {

    public static class MindJavascriptConsoleWindow extends JavascriptConsoleWindow {

        public MindJavascriptConsoleWindow(OCMind mind) {
            super();
            
            exposeObject("mind", mind);
            
            inputField.setBackground(Color.BLACK);
            inputField.setForeground(Color.ORANGE);
            
            textArea.setBackground(Color.BLACK);
            textArea.setForeground(Color.LIGHT_GRAY);
        }
    
        
    }
    
    public static void main(String[] args) {
        OCMind m = new OCMind();
        //new AtomizeXML(m, "/tmp/x.xml");
        new GraphStreamInput(m, "x_", "file:///tmp/x.json");
        
        m.addAgent(new LearnHebbian());        
        m.addAgent(new SpreadImportance());
        m.addAgent(new DecaySTI(0.5, (short)1));
        m.addAgent(new Forget(0.5, 20000, 40000));
        
        m.addAgent(new AddRandomHebbianEdges(0.5, 64, 8, 4000, 5000));
        m.addAgent(new RandomStimulation(0.5, (short)200, 3));
        m.addAgent(new MessageTokenizer(0.5));
        
        //new MindJavascriptConsoleWindow(m);
        new AttentionControlPanel(m, 0.75).newWindow();          
        new SwingWindow(new GraphPanel(new GraphView2D(m)), 800, 800, true);

        m.start(0.05);

    }
}
