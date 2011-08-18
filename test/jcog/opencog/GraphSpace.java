/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog;

import jcog.opencog.swing.AttentionControlPanel;
import jcog.opencog.swing.GraphPanel;
import jcog.opencog.swing.GraphView2D;
import jcog.opencog.swing.graph.BasicGraphView2DRenderer;
import jcog.opencog.swing.graph.GraphViewProcess;
import jcog.opencog.swing.graph.HyperassociativeMapLayout;
import jcog.spacegraph.swing.SwingWindow;

/**
 *
 * @author seh
 */
public class GraphSpace {
    
    public GraphSpace(OCMind mind) {
        super();        
        
        //new MindJavascriptConsoleWindow(m);

        new AttentionControlPanel(mind, 0.75).newWindow();          
        
        GraphViewProcess layout = new HyperassociativeMapLayout();
        //GraphViewProcess layout = new FDLayout();
        
        GraphView2D gv = new GraphView2D(mind, new BasicGraphView2DRenderer(), new GraphView2D.SeHGraphViewModel1(mind), layout);
        new SwingWindow(new GraphPanel(gv), 800, 800, true);
    }
}
