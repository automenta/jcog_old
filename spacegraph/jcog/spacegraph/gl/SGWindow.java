package jcog.spacegraph.gl;

import jcog.spacegraph.gl.SG;
import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLProfile;
import javax.media.opengl.awt.GLCanvas;
import com.sun.opengl.util.Animator;

/** Contains a "heavyweight" GLCanvas component for high performance */
public class SGWindow {

    private final Frame frame;
    private final Animator animator;

    public SGWindow(String title, SG sg) {
        GLCapabilities caps = new GLCapabilities(GLProfile.get(GLProfile.GL2));
        GLCanvas canvas = new GLCanvas(caps);

        frame = new Frame(title);
        frame.setLayout(new BorderLayout());
        canvas.addGLEventListener(sg);
        
        canvas.addMouseListener(sg);
        canvas.addMouseMotionListener(sg);
        canvas.addMouseWheelListener(sg);
        canvas.addKeyListener(sg);

        frame.add(canvas, BorderLayout.CENTER);

        frame.setSize(1024, 800);
        animator = new Animator(canvas);        
        
        frame.addWindowListener(new WindowAdapter() {

            public void windowClosing(WindowEvent e) {
                // Run this on another thread than the AWT event queue to
                // make sure the call to Animator.stop() completes before
                // exiting
                new Thread(new Runnable() {
                    public void run() {
                        animator.stop();
                        System.exit(0);
                    }
                }).start();
            }
        });
        frame.setVisible(true);
        animator.start();
    }



}
