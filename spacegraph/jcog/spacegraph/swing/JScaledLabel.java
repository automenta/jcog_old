/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.spacegraph.swing;

import javax.swing.JLabel;

/**
 *
 * @author seh
 */
public class JScaledLabel extends JLabel {

    public JScaledLabel(String s, float fontScale) {
        super(s);
        setFont(getFont().deriveFont((float)(getFont().getSize() * fontScale)));
    }


}
