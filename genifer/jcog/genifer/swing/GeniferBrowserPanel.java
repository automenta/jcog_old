/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.genifer.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import javax.swing.DefaultListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import jcog.genifer.Fact;
import jcog.genifer.Genifer;
import jcog.genifer.Rule;
import jcog.genifer.Truth;

/**
 *
 * @author seh
 */
public class GeniferBrowserPanel extends JPanel {
    private JList factList;
    private DefaultListModel factModel;
    private JList ruleList;
    private DefaultListModel ruleModel;
    private final Genifer genifer;

    static class FactRenderer implements ListCellRenderer {

        @Override
        public Component getListCellRendererComponent(final JList list, final Object value, final int index, final boolean isSelected, boolean cellHasFocus) {
            final JLabel l = new JLabel(value.toString());
            final Truth t = ((Fact)value).truth;
            l.setFont(l.getFont().deriveFont( getFontSize(t.getProbability())) );
            l.setForeground(getColor(t.getProbability()));
            return l;
        }
        
        public float getFontSize(double p) {
            return 10.0f + (float)p * 10.0f;
        }
        
        public Color getColor(double probability) {
            float a = 1.0f - (0.5f + ((float)probability)/2.0f);
            Color c = new Color(a, a, a);
            return c;
        }
        
    }
    
    static class RuleRenderer implements ListCellRenderer {

        @Override
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
            JLabel l = new JLabel(value.toString());
            return l;
        }

        
    }
    
    public GeniferBrowserPanel(Genifer g) {
        super();

        this.genifer = g;        
                
        refresh();
    }
    
    public synchronized void refresh() {
        removeAll();
        
        setLayout(new GridLayout(1, 2));
        
        
        factModel = new DefaultListModel();
        factList = new JList(factModel);
        factList.setCellRenderer(new FactRenderer());

        ruleModel = new DefaultListModel();
        ruleList = new JList(ruleModel);
        ruleList.setCellRenderer(new RuleRenderer());
        
        refreshFacts();
        refreshRules();

        add(new JScrollPane(factList));
        add(new JScrollPane(ruleList));

        updateUI();
    }

    public static class FactTruthComparator implements Comparator<Fact> {

        @Override
        public int compare(Fact a, Fact b) {
            double ap = a.truth.getProbability();
            double bp = b.truth.getProbability();
            if (ap < bp) return 1;
            else if (ap > bp) return -1;
            return 0;
        }
        
    }
    public static class RuleWComparator implements Comparator<Rule> {

        @Override
        public int compare(Rule a, Rule b) {
            double ap = a.getW();
            double bp = b.getW();
            if (ap < bp) return 1;
            else if (ap > bp) return -1;
            return 0;
        }
        
    }
    
    public void refreshFacts() {
        factModel.clear();

        List<Fact> lf = new ArrayList(genifer.getMemory().getFacts());
        Collections.sort(lf, new FactTruthComparator());
        for (Fact f : lf) {
            factModel.addElement(f);
        }
    }

    public void refreshRules() {
        ruleModel.clear();
        
        List<Rule> lf = new ArrayList(genifer.getMemory().getRules());
        Collections.sort(lf, new RuleWComparator());
        for (Rule f : lf) {
            ruleModel.addElement(f);
        }

    }

}