/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.spacegraph.swing;

import java.awt.FlowLayout;
import javax.swing.JPanel;

/**
 *
 * @author seh
 */
public class TransparentFlowPanel extends JPanel {

    public TransparentFlowPanel() {
        super(new FlowLayout(FlowLayout.LEFT));
//        super();
//        setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));
        setOpaque(false);
    }

}
