/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author seh
 */
package jcog.spacegraph.gl;

import java.awt.BorderLayout;
import java.awt.event.ComponentEvent;
import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLProfile;
import com.sun.opengl.util.Animator;
import java.awt.event.ComponentListener;
import javax.media.opengl.awt.GLCanvas;
import javax.swing.JPanel;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

public class SGPanel extends JPanel implements ComponentListener {

    private final Animator animator;
    private boolean running = false;
    private final GLCanvas canvas;
    private final Surface sg;

    public SGPanel(Surface sg) {
        super(new BorderLayout());

        this.sg = sg;

        GLCapabilities caps = new GLCapabilities(GLProfile.get(GLProfile.GL2));
        canvas = new GLCanvas(caps);
        //canvas = new GLJPanel(caps); /** a "lightweight" GLJPanel component for cooperation with other Swing components. */


        canvas.addGLEventListener(sg);

        canvas.addMouseListener(sg);
        canvas.addMouseMotionListener(sg);
        canvas.addMouseWheelListener(sg);
        canvas.addKeyListener(sg);

        animator = new Animator(canvas);
        animator.setRunAsFastAsPossible(false);



        addComponentListener(this);

        addAncestorListener(new AncestorListener() {

            @Override
            public void ancestorAdded(AncestorEvent event) {
                //System.out.println("ancestor added: " + event);
                setGLRunning(true);
            }

            @Override
            public void ancestorRemoved(AncestorEvent event) {
                //System.out.println("ancestor removed: " + event);
                setGLRunning(false);
            }

            @Override
            public void ancestorMoved(AncestorEvent event) {
            }
        });


    }

    public synchronized void setGLRunning(boolean trueToStart) {
        if (trueToStart) {
            if (!running) {
                //System.out.println("STARTING GL");

                running = true;

                if (animator.isAnimating()) {
                    animator.stop();
                }

                removeAll();

                add(canvas, BorderLayout.CENTER);
                updateUI();

                new Thread(new Runnable() {

                    public void run() {


                        animator.start();
                    }
                }).start();
            }
        } else {
            if (running) {
                //System.out.println("STOPPING GL");

                running = false;

                // Run this on another thread than the AWT event queue to make sure the call to Animator.stop() completes before exiting
//                new Thread(new Runnable() {
//                    public void run() {
                animator.stop();
                removeAll();
//                    }
//                }).start();
            }

        }
    }

    @Override
    public void componentResized(ComponentEvent e) {
    }

    @Override
    public void componentMoved(ComponentEvent e) {
    }

    @Override
    public void componentShown(ComponentEvent e) {
    }

    @Override
    public void componentHidden(ComponentEvent e) {
    }

    public Surface getSurface() {
        return sg;
    }

    public GLCanvas getCanvas() {
        return canvas;
    }

    public Animator getAnimator() {
        return animator;
    }
}
