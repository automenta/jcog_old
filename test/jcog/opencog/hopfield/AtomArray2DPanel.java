/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog.hopfield;

import edu.uci.ics.jung.graph.util.Pair;
import java.awt.Color;
import java.awt.GridLayout;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JFrame;
import javax.swing.JPanel;
import jcog.opencog.Atom;
import jcog.opencog.OCMind;

/**
 *
 * @author seh
 */
public class AtomArray2DPanel extends JPanel {
    Map<Atom, JPanel> panels = new HashMap();
    final AtomArray2D atomarray;
    private final OCMind mind;
    
    public AtomArray2DPanel(AtomArray2D a, OCMind mind) {
        super();
        
        setLayout(new GridLayout(a.width, a.height));
        
        this.atomarray = a;
        this.mind = mind;
        
        for (Atom x : a.atoms) {
            JPanel p = new JPanel();
            panels.put(x, p);
            add(p);
        }
        
        refresh();
    }

    public Color getColor(Atom a, short minSTI, short maxSTI) {
        short sti = mind.getSTI(a);
        float x = ((float)(sti - minSTI)) / ((float)(maxSTI-minSTI));
        return new Color(x, x, x);
    }

    public void refresh() {
        Pair<Short> stiRange = mind.getSTIRange(atomarray.atoms);
        short minSTI = stiRange.getFirst();
        short maxSTI = stiRange.getSecond();
        
        for (Atom x : atomarray.atoms) {
            JPanel p = panels.get(x);
            p.setBackground(getColor(x, minSTI, maxSTI));
        }
    }

    public JFrame newWindow() {
        final JFrame jf = new JFrame(getClass().getSimpleName());
        jf.getContentPane().add(this);
        jf.setSize(600, 600);
        jf.setVisible(true);
        
        final long period = 500;
        new Thread(new Runnable() {
            @Override
            public void run() {
                while (jf.isVisible()) {
                    refresh();
                    try {
                        Thread.sleep(period);
                    } catch (InterruptedException ex) {
                    }
                }
            }                            
        }).start();
        return jf;
    }
}
