/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.spacegraph;

import jcog.spacegraph.ui.GridRect;
import jcog.spacegraph.ui.PointerLayer;
import jcog.spacegraph.ui.Button;
import jcog.spacegraph.swing.SwingWindow;

/**
 *
 * @author seh
 */
public class DemoButton extends AbstractSurfaceDemo {

    @Override
    public String getName() {
        return "Demo Button";
    }

    @Override
    public String getDescription() {
        return "Demo Button";
    }

    public DemoButton() {
        super();

        add(new GridRect(6, 6));

        int w = 1;
        int h = 1;
        for (int x = -w; x <= w; x++) {
            for (int y = -h; y <= h; y++) {
                add(new Button().scale(0.9f, 0.9f).center(x, y));
            }            
        }


        add(new PointerLayer(this));
    }

    public static void main(String[] args) {
        new SwingWindow(AbstractSurfaceDemo.newPanel(new DemoButton()), 800, 800, true);
    }
}
