/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.hopfield;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import jcog.opencog.Atom;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.statistics.HistogramDataset;
import org.jfree.data.statistics.HistogramType;

/**
 * Panel for manually invoking mindagents
 * @author seh
 */
public class AgentControlPanel extends JPanel {

    private final OCMind mind;
    private final JButton cycle;
    private HistogramDataset stiDataset;
    private JFreeChart stiHistogram;
    private ChartPanel stiChart;
    Map<MindAgent, MindAgentPanel> mindAgentPanels = new WeakHashMap();

    /** panel representing a mind agent:
     *   --checkbox indicating whether it will be included in a cycle
     *   --button for immediate invocation
     *   --indicator for last execution time (changes color of background)
     */
    public class MindAgentPanel extends JPanel {

        public final JCheckBox cycleEnabled;
        public final MindAgent agent;

        public MindAgentPanel(final MindAgent m) {
            super(new FlowLayout());

            this.agent = m;

            cycleEnabled = new JCheckBox();
            cycleEnabled.setSelected(true);
            add(cycleEnabled);

            JMenu nameMenu = new JMenu(m.toString());
            {
                JMenuItem runNowItem = new JMenuItem("Run Now");
                runNowItem.addActionListener(new ActionListener() {

                    @Override
                    public void actionPerformed(ActionEvent ae) {
                        runAgent(MindAgentPanel.this);
                    }
                });
                
                nameMenu.add(runNowItem);
                
                nameMenu.addSeparator();
                
                for (final Method a : m.getClass().getMethods()) {
                    if (Modifier.isPublic(a.getModifiers())) {
                        if (a.getParameterTypes().length == 0) {
                            JMenuItem j = new JMenuItem(a.getName());
                            j.addActionListener(new ActionListener() {
                                @Override
                                public void actionPerformed(ActionEvent e) {
                                    try {
                                        a.invoke(m);
                                    } catch (IllegalAccessException ex) {
                                        Logger.getLogger(AgentControlPanel.class.getName()).log(Level.SEVERE, null, ex);
                                    } catch (IllegalArgumentException ex) {
                                        Logger.getLogger(AgentControlPanel.class.getName()).log(Level.SEVERE, null, ex);
                                    } catch (InvocationTargetException ex) {
                                        Logger.getLogger(AgentControlPanel.class.getName()).log(Level.SEVERE, null, ex);
                                    }
                                }                                
                            });
                            nameMenu.add(j);
                        }
                    }
                }
            }
            
            JMenuBar jb = new JMenuBar();
            jb.add(nameMenu);
            add(jb);

        }
    }

    public AgentControlPanel(OCMind mind) {
        super(new BorderLayout());

        this.mind = mind;

        cycle = new JButton("Cycle");
        cycle.setMnemonic('c');
        cycle.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                SwingUtilities.invokeLater(new Runnable() {

                    @Override
                    public void run() {
                        cycle();
                    }
                });
            }
        });


        refresh();
    }

    protected MindAgentPanel getMindAgentPanel(MindAgent m) {
        MindAgentPanel map = mindAgentPanels.get(m);
        if (map == null) {
            map = new MindAgentPanel(m);
            mindAgentPanels.put(m, map);
        }
        return map;
    }

    protected void refresh() {
        removeAll();

        JPanel agentPanel = new JPanel(new BorderLayout());
        {
            JPanel agentList = new JPanel();
            agentList.setLayout(new BoxLayout(agentList, BoxLayout.PAGE_AXIS));
            {
                for (MindAgent m : mind.getAgents()) {
                    agentList.add(getMindAgentPanel(m));
                }
                agentList.add(Box.createVerticalBox());

            }
            agentPanel.add(new JScrollPane(agentList), BorderLayout.CENTER);

            agentPanel.add(cycle, BorderLayout.SOUTH);
        }

        JPanel statsPanel = new JPanel();
        statsPanel.setLayout(new BoxLayout(statsPanel, BoxLayout.PAGE_AXIS));
        {

            List<Atom> stimulatedAtoms = new LinkedList();
            for (MindAgent m : mind.getAgents()) {
                stimulatedAtoms.addAll(m.getStimulated());
            }
            if (stimulatedAtoms.size() > 0) {
                double[] stis = new double[stimulatedAtoms.size()];
                double[] ltis = new double[stimulatedAtoms.size()];
                int i = 0;
                for (Atom a : stimulatedAtoms) {
                    stis[i] = mind.getSTI(a);
                    ltis[i] = mind.getLTI(a);
                    i++;
                }
                stiDataset = new HistogramDataset();

                stiDataset.setType(HistogramType.FREQUENCY);
                stiDataset.addSeries("STI", stis, 20);
                stiDataset.addSeries("LTI", ltis, 20);

                stiHistogram = ChartFactory.createHistogram("Attention Distribution of Agent-Stimulated Atoms", "Importance", "Count", stiDataset, PlotOrientation.VERTICAL, false, false, false);
                stiHistogram.getXYPlot().setForegroundAlpha(0.75f);

                stiChart = new ChartPanel(stiHistogram, true, true, true, true, true);

                statsPanel.add(stiChart);
            } else {
            }
        }

        add(agentPanel, BorderLayout.WEST);
        add(new JScrollPane(statsPanel), BorderLayout.CENTER);

        updateUI();
    }

    protected void runAgent(MindAgentPanel m) {
        //TODO calculate runtime of each agent to display
        m.agent._run(mind, 1.0);
    }

    protected void cycle() {        
        for (MindAgent m : mind.getAgents()) {
            MindAgentPanel map = getMindAgentPanel(m);
            if (map.cycleEnabled.isSelected()) {
                runAgent(map);
            }
        }

        refresh();
    }

    public JFrame newWindow() {
        final JFrame jf = new JFrame(getClass().getSimpleName());
        jf.getContentPane().add(this);
        jf.setSize(800, 600);
        jf.setVisible(true);

        return jf;
    }
}
