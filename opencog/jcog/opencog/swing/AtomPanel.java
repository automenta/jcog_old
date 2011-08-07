/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import javax.swing.JPanel;
import jcog.opencog.Atom;
import jcog.opencog.OCMind;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.LineAndShapeRenderer;
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.category.DefaultCategoryDataset;

/**
 *
 * @author seh
 */
public class AtomPanel extends JPanel {
    final DefaultCategoryDataset dataset = new DefaultCategoryDataset();
    int time = 0;
    final Atom atom;
    final OCMind mind;

    public AtomPanel(final OCMind mind, final Atom a) {
        this.atom = a;
        this.mind = mind;
        final JFreeChart chart = createChart(dataset);
        final ChartPanel chartPanel = new ChartPanel(chart);
        //chartPanel.setPreferredSize(new Dimension(500, 270));
        add(chartPanel, BorderLayout.CENTER);
    }

    public void refresh() {
        dataset.addValue((double) mind.getSTI(atom), "STI", (Integer) time);
        //dataset.addValue((double)mind.getLTI(atom), "LTI", (Integer)time);
        time++;
    }

    private JFreeChart createChart(final CategoryDataset dataset) {
        // create the chart...
        final JFreeChart chart = ChartFactory.createLineChart(mind.getName(atom), "Time", "Importance", dataset, PlotOrientation.VERTICAL, false, true, false);
        // NOW DO SOME OPTIONAL CUSTOMISATION OF THE CHART...
        //        final StandardLegend legend = (StandardLegend) chart.getLegend();
        //      legend.setDisplaySeriesShapes(true);
        //    legend.setShapeScaleX(1.5);
        //  legend.setShapeScaleY(1.5);
        //legend.setDisplaySeriesLines(true);
        chart.setBackgroundPaint(Color.white);
        final CategoryPlot plot = (CategoryPlot) chart.getPlot();
        plot.setBackgroundPaint(Color.BLACK);
        plot.setRangeGridlinePaint(Color.white);
        // customise the range axis...
        final NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
        rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
        rangeAxis.setAutoRangeIncludesZero(true);
        
        final LineAndShapeRenderer renderer = (LineAndShapeRenderer) plot.getRenderer();
                //renderer.setDrawShapes(true);
        
        renderer.setSeriesStroke(
                0, new BasicStroke(
                2.0f));
        renderer.setSeriesPaint(0, Color.WHITE);
        
        //            renderer.setSeriesStroke(
        //                    1, new BasicStroke(
        //                    2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
        //                    1.0f, new float[]{6.0f, 6.0f}, 0.0f));
        //            renderer.setSeriesStroke(
        //                    2, new BasicStroke(
        //                    2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
        //                    1.0f, new float[]{2.0f, 6.0f}, 0.0f));
        // OPTIONAL CUSTOMISATION COMPLETED.
        return chart;
    }
    
}
