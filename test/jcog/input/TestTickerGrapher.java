/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import jcog.graphstream.GraphStream;
import jcog.input.calais.CalaisTag;
import jcog.input.calais.CalaisTopic;
import jcog.input.finance.MarketIndicator;
import jcog.input.finance.Ticker;
import jcog.input.finance.TickerGraphing;
import jcog.input.finance.TickerPoint;
import jcog.input.finance.TickerSource;
import jcog.opencog.OCMind;
import jcog.spacegraph.swing.SwingWindow;
import org.encog.util.time.DateUtil;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.DefaultCategoryDataset;

/**
 *
 * @author seh
 */
public class TestTickerGrapher {

    public static abstract class VirtualTicker extends Ticker {
        public final List<Ticker> tickers;

        //assumes that each Ticker in 'tickers' has saem from->to dates
        public VirtualTicker(String symbol, List<Ticker> tickers) {
            super(symbol, tickers.get(0).from, tickers.get(0).to);
                        
            this.tickers = tickers;
        }
        
        protected void init(Map<Ticker,Double> sources) {
            Map<Date, TickerPoint> tp = new HashMap();
            for (Ticker x : sources.keySet()) {
                for (TickerPoint y : x.data) {
                    final double min = x.getMin(MarketIndicator.CLOSE);
                    final double max = x.getMax(MarketIndicator.CLOSE);
                    TickerPoint l = tp.get(y.getWhen());
                    if (l == null) {
                        l = new TickerPoint(y.getWhen(), "");
                        l.setData(MarketIndicator.CLOSE, y.getNormalizedData(MarketIndicator.CLOSE, min, max) * sources.get(x));
                        tp.put(y.getWhen(), l);
                        data.add(l);
                    }
                    else {
                        double prevValue = l.getData(MarketIndicator.CLOSE);
                        double add = prevValue + y.getNormalizedData(MarketIndicator.CLOSE, min, max) * sources.get(x);
                        l.setData(MarketIndicator.CLOSE, add);                        
                    }
                }
            }
            
            String contained = "";
            for (Ticker x : sources.keySet()) {
                contained += x.symbol + " ";
            }
            this.symbol = getPrefix() + ":" + this.symbol + " [ " + contained + "]";

        }

        abstract public String getPrefix();
        abstract protected Map<Ticker, Double> getInfluences();
        
    }
    public static class VirtualTopicTicker extends VirtualTicker {
        private final CalaisTopic t;
        
        public VirtualTopicTicker(CalaisTopic t, List<Ticker> tickers) {
            super(t.name, tickers);

            this.t = t;            
            
            init(getInfluences());
            
        }
        @Override public String getPrefix() { return "topic"; }

        @Override
        protected Map<Ticker, Double> getInfluences() {
            Map<Ticker, Double> sources = new HashMap();
            for (Ticker ti : tickers) {
                CalaisTopic found = ti.getTopic(t.id);
                if (found!=null)
                    sources.put(ti, found.score);
            }
            return sources;
        }
        
    }
    
    public static class VirtualTagTicker extends VirtualTicker {
        private final CalaisTag t;

        public VirtualTagTicker(CalaisTag t, List<Ticker> tickers) {
            super(t.name, tickers);

            this.t = t;            
            
            init(getInfluences());            
            
        }

        @Override public String getPrefix() { return "tag"; }
        
        @Override protected Map<Ticker, Double> getInfluences() {
            Map<Ticker, Double> sources = new HashMap();
            for (Ticker ti : tickers) {
                //System.out.println(t.name + " in? " + ti.tags);
                
                CalaisTag found = ti.getTag(t.name);
                if (found!=null) {
                    sources.put(ti, found.importance);
                    System.out.println("  " + t.name + "  found in " + ti.symbol + " with " + found.importance);
                }
            }
            System.out.println(t.name + "=" + sources);
            return sources;
        }
        
    }
    
    public static class TickerPlotter extends JPanel {

        private final JPanel sideMenu;
        private final JPanel chartPanel;
        private final TickerGraphing tg;

        public TickerPlotter(TickerGraphing tg) {
            super(new BorderLayout());

            this.tg = tg;

            sideMenu = new JPanel();
            sideMenu.setLayout(new BoxLayout(sideMenu, BoxLayout.Y_AXIS));
            add(new JScrollPane(sideMenu), BorderLayout.WEST);

            chartPanel = new JPanel();
            chartPanel.setLayout(new BoxLayout(chartPanel, BoxLayout.Y_AXIS));
            add(new JScrollPane(chartPanel), BorderLayout.CENTER);

            refreshMenu();
        }

        public JComponent newChart(Ticker t) {
            JPanel p = new JPanel(new BorderLayout());

            final DefaultCategoryDataset dataset = new DefaultCategoryDataset();
            for (TickerPoint dp : t.data) {  
                dataset.addValue( dp.getData(MarketIndicator.CLOSE), "Close Price", dp.getWhen());
            }

            JFreeChart chart = ChartFactory.createLineChart(
                    t.symbol, // chart title
                    "Date", // domain axis label
                    "Value", // range axis label
                    dataset, // data
                    PlotOrientation.VERTICAL, // orientation
                    false, // include legend
                    true, // tooltips
                    false // urls
                    );

            // NOW DO SOME OPTIONAL CUSTOMISATION OF THE CHART...
//        final StandardLegend legend = (StandardLegend) chart.getLegend();
            //      legend.setDisplaySeriesShapes(true);
            //    legend.setShapeScaleX(1.5);
            //  legend.setShapeScaleY(1.5);
            //legend.setDisplaySeriesLines(true);

            chart.setBackgroundPaint(Color.white);

            final CategoryPlot plot = (CategoryPlot) chart.getPlot();
            plot.setBackgroundPaint(Color.lightGray);
            plot.setRangeGridlinePaint(Color.white);

            // customise the range axis...
            final NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
            rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
            rangeAxis.setAutoRangeIncludesZero(true);


            // customise the renderer...
//            final LineAndShapeRenderer renderer = (LineAndShapeRenderer) plot.getRenderer();
//        renderer.setDrawShapes(true);

//            renderer.setSeriesStroke(
//                    0, new BasicStroke(
//                    2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
//                    1.0f, new float[]{10.0f, 6.0f}, 0.0f));
//            renderer.setSeriesStroke(
//                    1, new BasicStroke(
//                    2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
//                    1.0f, new float[]{6.0f, 6.0f}, 0.0f));
//            renderer.setSeriesStroke(
//                    2, new BasicStroke(
//                    2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND,
//                    1.0f, new float[]{2.0f, 6.0f}, 0.0f));
//            // OPTIONAL CUSTOMISATION COMPLETED.

            p.add(new ChartPanel(chart), BorderLayout.CENTER);


            return p;
        }

        protected void addTicker(final Ticker t) {
            final JCheckBox jb = new JCheckBox(t.symbol);
            sideMenu.add(jb);

            jb.addActionListener(new ActionListener() {

                JComponent chart = null;

                @Override
                public void actionPerformed(ActionEvent e) {
                    SwingUtilities.invokeLater(new Runnable() {

                        @Override
                        public void run() {
                            if (jb.isSelected()) {
                                chart = newChart(t);
                                chartPanel.add(chart);
                                chartPanel.updateUI();
                            } else {
                                if (chart != null) {
                                    chartPanel.remove(chart);
                                    chartPanel.updateUI();
                                    chart = null;
                                }
                            }
                        }
                    });
                }
            });
        }

        protected void refreshMenu() {
            sideMenu.removeAll();

            for (final Ticker t : tg.tickers) {
                addTicker(t);
            }
            for (final CalaisTag t : tg.tags.values()) {
                addTicker(new VirtualTagTicker(t, tg.tickers));
            }
            for (final CalaisTopic t : tg.topics.values()) {
                addTicker(new VirtualTopicTicker(t, tg.tickers));
            }


//                for (CalaisTag tag : t.tags) {
//                }
//                for (CalaisEntity ent : t.entities) {
//                }
//                for (CalaisTopic top : t.topics) {
//                }
            

            updateUI();
        }
    }

    public static void main(String[] args) {
        Date startDate = DateUtil.createDate(1, 4, 2011);
        Date today = new Date();

        String cachePath = "/tmp/" + TestTicker.class.getSimpleName() + ".cache";

        List<Ticker> tickers = new LinkedList();

        //http://biz.yahoo.com/ic/ind_index.html
        String[] symbols = new String[]{"IBM", "AAPL", "MSFT", "GOOG", "INTC", "AMD", "NTDOY", "SNE", "ATVI", "^NYA", "ORCL", "RHT", "RIM.TO", "NSM"};
        final TickerSource source = new TickerSource(cachePath);


        try {
            for (String s : symbols) {
                tickers.add(source.load(s, startDate, today));
            }

            final OCMind m = new OCMind();

            final TickerGraphing tg = new TickerGraphing(m, tickers);

            //m.printAtoms();

            new SwingWindow(new TickerPlotter(tg), 800, 600, true);
            new GraphStream(m, 8000);

        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }
}
