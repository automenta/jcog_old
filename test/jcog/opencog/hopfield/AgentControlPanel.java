/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog.hopfield;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;

/**
 * Panel for manually invoking mindagents
 * @author seh
 */
public class AgentControlPanel extends JPanel {
    private final OCMind mind;

    public AgentControlPanel(OCMind mind) {
        super(new BorderLayout());
        
        this.mind = mind;
        
        refresh();
    }
    
    protected void refresh() {
        removeAll();
        JPanel agentList = new JPanel();
        agentList.setLayout(new BoxLayout(agentList, BoxLayout.PAGE_AXIS));
        {
            for (MindAgent m : mind.getAgents()) {
                //TODO make a checkbox to enable/disable
                agentList.add(new JLabel(m.toString()));
            }        
            
        }
        add(new JScrollPane(agentList), BorderLayout.CENTER);
        
        JButton cycle = new JButton("Cycle");
        cycle.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent ae) {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        cycle();
                        refresh();
                    }            
                });
            }            
        });
        add(cycle, BorderLayout.SOUTH);
        updateUI();
    }
    
    protected void cycle() {
        for (MindAgent m : mind.getAgents()) {
            //TODO calculate runtime of each agent to display
            m.run(mind);
        }
    }

    public JFrame newWindow() {
        final JFrame jf = new JFrame(getClass().getSimpleName());
        jf.getContentPane().add(this);
        jf.setSize(400, 400);
        jf.setVisible(true);

        return jf;
    }
    
}
