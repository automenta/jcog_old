/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.spacegraph;

import automenta.spacegraph.swing.SwingWindow;
import jcog.spacegraph.control.Pointer;
import jcog.spacegraph.ui.GridRect;
import jcog.spacegraph.ui.PointerLayer;
import jcog.spacegraph.math.linalg.Vec4f;
import jcog.spacegraph.ui.Button;
import jcog.spacegraph.ui.TextButton;
import java.awt.Color;

/**
 *
 * @author seh
 */
public class DemoIntersectRect extends AbstractSurfaceDemo {

    @Override
    public String getName() {
        return "2D Fractal Surface";
    }

    @Override
    public String getDescription() {
        return "Zoomable fractal 2D surface.  Multiple adjustable layers.  Adjustable control logic.";
    }


    public DemoIntersectRect() {
        super();

        add(new GridRect(6, 6));
        
        add(new TextButton("Abc") {

            @Override
            public void onTouchChange(Pointer pointer, boolean touched) {
                if (touched) {
                    setText("Xyz");
                }
                else {
                    setText("Abc");
                }
                
            }

            
        }.color(0.1f, 0.2f, 0.6f, 0.5f));

        /* add rectangles, testing:
                --position
                --size
                --color
                --tilt
         */
        int numRectangles = 24;
        float maxRadius = 0.1f;
        for (int i = 0; i < numRectangles; i++) {
            float s = 1.0f + (float)Math.random() * maxRadius;
            float a = (float)i;
            float x = (float)Math.cos(a) * s;
            float y = (float)Math.sin(a) * s;
            
            Button r2 = new Button() {

                @Override
                public void onTouchChange(Pointer pointer, boolean touched) {
                    color(touched ? new Vec4f(Color.MAGENTA) : new Vec4f(Color.ORANGE));
                }
                
                
            };
            r2.color(0.6f, 0.2f, 0.1f, 0.5f);
            
            r2.move(x, y, 0);
            r2.scale(0.6f, 0.3f);
            r2.tilt(a);
            add(r2);
            
        }

        add(new PointerLayer(this));
    }
    
    

    public static void main(String[] args) {
        new SwingWindow(AbstractSurfaceDemo.newPanel(new DemoIntersectRect()), 800, 800, true);
    }
}
