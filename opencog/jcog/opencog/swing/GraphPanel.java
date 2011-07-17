/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing;

import java.awt.BorderLayout;
import javax.swing.JPanel;
import jcog.spacegraph.control.FractalControl;
import jcog.spacegraph.gl.SGPanel;
import jcog.spacegraph.gl.Surface;

/**
 *
 * @author seh
 */
public class GraphPanel extends JPanel {

    public GraphPanel(Surface space) {
        super(new BorderLayout());
        
        SGPanel sdc = new SGPanel(space);
        new FractalControl(sdc);
        //final ControlRigPanel crp = new ControlRigPanel(space, 0.25f);
        //new Thread(crp).start();
        add(sdc, BorderLayout.CENTER);
        //j.add(crp, BorderLayout.SOUTH);
    }
    
}
