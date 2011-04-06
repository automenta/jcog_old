/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.spacegraph;

import automenta.spacegraph.swing.SwingWindow;
import jcog.spacegraph.control.Pointer;
import jcog.spacegraph.control.Repeat;
import jcog.spacegraph.ui.PointerLayer;
import jcog.spacegraph.math.linalg.Vec4f;
import jcog.spacegraph.ui.Button;
import java.awt.Color;

/**
 *
 * @author seh
 */
public class DemoRectTilt extends AbstractSurfaceDemo {

    @Override
    public String getName() {
        return "2D Fractal Surface";
    }

    @Override
    public String getDescription() {
        return "Tests intersection with a Rect.";
    }

    public DemoRectTilt() {
        super();

        //add(new GridRect(6, 6));

        /* add rectangles, testing:
        --position
        --size
        --color
        --tilt
         */
        float a = 0.5f;
        float x = 2;
        float y = 0;

        final Button r2 = new Button();

        add(new Repeat() {
            @Override
            public void update(double dt, double t) {
                r2.tilt(r2.getTilt() + 0.04f);
                float s = (float)Math.sin(t*4.0) + 2.0f;
                r2.scale(1.5f * s, s);
            }
        });
        
        r2.move(x, y, 0);
        r2.scale(0.9f, 0.4f);
        r2.tilt(a);
        add(r2);

        add(new PointerLayer(this));
    }

    public static void main(String[] args) {
        new SwingWindow(new DemoRectTilt().newPanel(), 800, 800, true);
    }
}
