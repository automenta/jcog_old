package jcog.spacegraph.swing;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JComponent;
import javax.swing.JFrame;

public class SwingWindow extends JFrame {

    public SwingWindow(JComponent j, int w, int h) {
        super();
        //getContentPane().add(j);
        setContentPane(j);
        setVisible(true);
        setSize(w, h);
    }

    public SwingWindow(JComponent j, int w, int h, boolean exitOnClose) {
        this(j, w, h);
        if (exitOnClose) {
            addWindowListener(new WindowAdapter() {
                @Override public void windowClosing(WindowEvent e) {
                    super.windowClosing(e);
                    onClosing();
                    System.exit(0);
                }
            });
        }
    }

    protected void onClosing() {
        
    }

}

