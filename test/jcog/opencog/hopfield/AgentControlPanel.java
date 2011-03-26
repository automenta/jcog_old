/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog.hopfield;

import edu.uci.ics.jung.graph.util.Pair;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Map;
import java.util.WeakHashMap;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
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
import org.jfree.data.statistics.SimpleHistogramBin;
import org.jfree.data.statistics.SimpleHistogramDataset;

/**
 * Panel for manually invoking mindagents
 * @author seh
 */
public class AgentControlPanel extends JPanel {
    private final OCMind mind;
    private final JButton cycle;
    
    private final SimpleHistogramDataset stiDataset;
    private final JFreeChart stiHistogram;
    private final ChartPanel stiChart;
    
    Map<MindAgent, MindAgentPanel> mindAgentPanels = new WeakHashMap();
    
    /** panel representing a mind agent:
     *   --checkbox indicating whether it will be included in a cycle
     *   --button for immediate invocation
     *   --indicator for last execution time (changes color of background)
     */
    public class MindAgentPanel extends JPanel {
        public final JCheckBox cycleEnabled;
        private final JButton nameButton;
        public final MindAgent agent;
        
        public MindAgentPanel(final MindAgent m) {
            super(new FlowLayout());
            
            this.agent = m;
            
            cycleEnabled = new JCheckBox();
            cycleEnabled.setSelected(true);
            add(cycleEnabled);
            
            nameButton = new JButton(m.toString());
            nameButton.addActionListener(new ActionListener() {
                @Override public void actionPerformed(ActionEvent ae) {
                    runAgent(MindAgentPanel.this);
                }                
            });
            add(nameButton);
            
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
                        refresh();
                    }            
                });
            }            
        });

        stiDataset = new SimpleHistogramDataset("STI");
        stiHistogram = ChartFactory.createHistogram("STI Distribution of Agent-Stimulated Atoms", "STI", "Count", stiDataset, PlotOrientation.VERTICAL, false, false, false);
        stiChart = new ChartPanel(stiHistogram, true, true, true, true, true);
        
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
            stiDataset.clearObservations();
            stiDataset.removeAllBins();
            
            int numBins = 10;
            Pair<Short> stiRange = mind.getSTIRange(mind.getVertices());
            short minSTI = stiRange.getFirst();
            short maxSTI = stiRange.getSecond();
            int interval = Math.max(1, 1 + (maxSTI - minSTI) / numBins);

            int x = minSTI;
            for (int i = 0; i < numBins; i++) {
                stiDataset.addBin(new SimpleHistogramBin(x, x + interval));
                x += interval+1;
            }
            for (MindAgent m : mind.getAgents())
                for (Atom a : m.getStimulated())
                    stiDataset.addObservation(mind.getSTI(a));
            stiHistogram.fireChartChanged();
            statsPanel.add(stiChart);
        }
        
        add(agentPanel, BorderLayout.WEST);
        add(new JScrollPane(statsPanel), BorderLayout.CENTER);
        
        updateUI();
    }
    
    protected void runAgent(MindAgentPanel m) {
        //TODO calculate runtime of each agent to display
        m.agent.run(mind);
    }
    
    protected void cycle() {
        for (MindAgent m : mind.getAgents()) {
            MindAgentPanel map = getMindAgentPanel(m);
            if (map.cycleEnabled.isSelected())
                runAgent(map);
        }
    }

    public JFrame newWindow() {
        final JFrame jf = new JFrame(getClass().getSimpleName());
        jf.getContentPane().add(this);
        jf.setSize(800, 600);
        jf.setVisible(true);

        return jf;
    }
    
}
