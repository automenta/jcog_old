/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input;

import com.syncleus.dann.graph.Graph;
import com.syncleus.dann.graph.ImmutableDirectedEdge;
import com.syncleus.dann.graphicalmodel.GraphicalModelNode;
import com.syncleus.dann.graphicalmodel.SimpleGraphicalModelNode;
import com.syncleus.dann.graphicalmodel.bayesian.MutableBayesianAdjacencyNetwork;
import edu.uci.ics.jung.algorithms.layout.ISOMLayout;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse;
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position;
import java.awt.Dimension;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import javax.swing.JFrame;
import jcog.atom.JungGraph;
import jcog.input.finance.MarketIndicator;
import jcog.input.finance.Ticker;
import jcog.input.finance.TickerPoint;
import jcog.input.finance.TickerSource;
import org.encog.util.time.DateUtil;

/**
 *
 * @author seh
 */
public class TestDynamicBayesMultipleTickers {
    
    
    public static class TickerBayesianNetwork {
        public final static Logger logger = Logger.getLogger(TickerBayesianNetwork.class.toString());
        
        public final MutableBayesianAdjacencyNetwork network = new MutableBayesianAdjacencyNetwork();
        private final int changeResolution;
        private final int historyLength;
        private final List<Ticker> ticker;
        
        private Map<Ticker, List<GraphicalModelNode<Integer>>> past = new HashMap(); //0 = previous change, 1 = previous previous, ...
        private Map<Ticker, GraphicalModelNode<Integer>> future = new HashMap();         
        private Map<Ticker, Double> maxValueChange = new HashMap();
        private Map<Ticker, List<TickerPoint>> tpl = new HashMap();
                
        public TickerBayesianNetwork(List<Ticker> lt, int changeResolution, int historyLength) {
            super();
            
            this.ticker = lt;
            
            this.changeResolution = changeResolution;
            this.historyLength = historyLength;
            
            for (Ticker t : lt) {
                past.put(t, new ArrayList(historyLength));
                
                for (int i = 0; i < historyLength; i++) {
                    GraphicalModelNode<Integer> ps = newPriceState(t.symbol + ".past(" + i + ")");                
                    past.get(t).add(ps);
                    network.add(ps);                
                }

                GraphicalModelNode<Integer> futureState = newPriceState(t.symbol + ".future");
                future.put(t, futureState);            

                network.add(futureState);
                
            }
            
            //add past->future edges
            for (Ticker t : lt) {
                List<GraphicalModelNode<Integer>> tp = past.get(t);

                for (int i = 0; i < historyLength; i++) {

                    for (int j = 1; j <= historyLength; j++) {
                        if (i - j >= 0) {
                            //System.out.println((i-j) + " <- " + i );
                            network.add(new ImmutableDirectedEdge<GraphicalModelNode>(tp.get(i), tp.get(i-j)));
                        }
                    }

                    //System.out.println("future <- " + i );
                    network.add(new ImmutableDirectedEdge<GraphicalModelNode>(tp.get(i), future.get(t)));
                }
                
                maxValueChange.put(t, t.getMaxChange(MarketIndicator.CLOSE));

            }
            System.out.println("max daily change in market close = " + maxValueChange);
            
            //add inter-ticker edges
            for (Ticker a : lt) {
                for (Ticker b : lt) {
                    if (a == b)
                        continue;
                    
                    for (int i = 1; i < historyLength; i++) {
                        network.add(new ImmutableDirectedEdge<GraphicalModelNode>(past.get(a).get(i), past.get(b).get(i-1)));
                    }
                    network.add(new ImmutableDirectedEdge<GraphicalModelNode>(past.get(a).get(0), future.get(b)));
                }                
            }

            
            int tplSize = -1;
            for (Ticker t : lt) {
                List<TickerPoint> _tpl = new ArrayList(t.data);
                if (_tpl.size() <= historyLength ) {
                    logger.severe("history of " + t + " not long enough for historyLength=" + historyLength );
                    return;
                }
                if (tplSize == -1) {
                    tplSize = _tpl.size();
                }
                else {
                    if (tplSize != _tpl.size()) {
                        logger.severe("history of Ticker " + t + " not consistent with previous tickers");
                        return;
                    }
                }
                tpl.put(t, _tpl);
            }
                        
            for (int i = historyLength+1; i < tplSize; i++) {
                for (Ticker t : lt) {
                   List<TickerPoint> ltp = tpl.get(t);
                   
                   final TickerPoint now = ltp.get(i);
                   final GraphicalModelNode<Integer> futureState = future.get(t);
                   
                   double d = now.getData(MarketIndicator.CLOSE) - ltp.get(i-1).getData(MarketIndicator.CLOSE);
                   futureState.setState(getChangeState(t, d));

                   System.out.println(ltp.get(i).getWhen() + " " + d + " " + getChangeState(t, d));

                   for (int j = 1; j <= historyLength; j++) {
                       System.out.println("  " + ltp.get(i-j-1).getWhen() + " to " + ltp.get(i-j).getWhen() + " into history " + (j-1) );
                       double e = ltp.get(i-j).getData(MarketIndicator.CLOSE) - ltp.get(i-j-1).getData(MarketIndicator.CLOSE);
                       past.get(t).get(j-1).setState(getChangeState(t, e));
                   }
                }
               
               network.learnStates();
            }
            
            displayGraph(network);
            
            //shift backwards
            Set<GraphicalModelNode> influences = new HashSet();

            for (final Ticker t : lt) {
                final GraphicalModelNode<Integer> futureState = future.get(t);
                for (int i = 1; i < past.get(t).size(); i++) {
                    past.get(t).get(i-1).setState(past.get(t).get(i).getState());
                }
                past.get(t).get(past.get(t).size()-1).setState(futureState.getState());                

                influences.add(past.get(t).get(past.get(t).size()-1)); //oldest past
                //influences.addAll(past.get(t));
            }
            
            
            for (final Ticker t : lt) {
                final GraphicalModelNode<Integer> futureState = future.get(t);
                
                Set<GraphicalModelNode> goals = new HashSet();
                goals.add(futureState);
                
                //Set<GraphicalModelNode> influences = new HashSet();
                //influences.addAll(past.get(t));

                System.out.println("PREDICTION: " + futureState + " <- (" + influences + ")");

                for (int i = -changeResolution; i <= changeResolution; i++) {
                    futureState.setState(i);

                    double p = network.conditionalProbability(goals, influences);

                    System.out.println(i + "=" + p);                
                }
            }
        }

        /** discretizes the price change */
        public int getChangeState(Ticker t, double change) {
            double prop = change / maxValueChange.get(t);
            return (int)Math.round(prop * changeResolution);
        }
        
        public GraphicalModelNode<Integer> newPriceState(final String label) {
            return new SimpleGraphicalModelNode<Integer>(0) {

                @Override
                public String toString() {
                    return label;
                }
                
            };
        }

    }
    
    public static void displayGraph(Graph g) {
        JungGraph jg = new JungGraph(g);
        
        //Layout layout = new FRLayout(jg, new Dimension(400,400));
        Layout layout = new ISOMLayout(jg);
        layout.setSize(new Dimension(400,400)); // sets the initial size of the space
        
        VisualizationViewer vv = new VisualizationViewer<Integer,String>(layout);
        
        vv.getRenderContext().setVertexLabelTransformer(new ToStringLabeller());
        //vv.getRenderContext().setEdgeLabelTransformer(new ToStringLabeller());
        vv.getRenderer().getVertexLabelRenderer().setPosition(Position.CNTR);
        vv.setPreferredSize(new Dimension(350,350)); //Sets the viewing area size
        
        DefaultModalGraphMouse gm = new DefaultModalGraphMouse();
        gm.setMode(ModalGraphMouse.Mode.TRANSFORMING);
        vv.setGraphMouse(gm);

        JFrame frame = new JFrame("Bayesian Graph");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.getContentPane().add(vv);
        frame.pack();
        frame.setVisible(true);

    }
    
    public static void main(String[] args) {
        TickerSource ts = TestTicker.getTickerSource();
        
        Date startDate = DateUtil.createDate(1, 4, 2011);
        Date endDate = DateUtil.createDate(9, 8, 2011);
        try {
            List<Ticker> ti = new LinkedList();
            
            ti.add( ts.load("IBM", startDate, endDate) );
            ti.add( ts.load("MSFT", startDate, endDate) );            
            ti.add( ts.load("AAPL", startDate, endDate) );            
            
            TickerBayesianNetwork tbn = new TickerBayesianNetwork(ti, 2, 2);
                        
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        
    }
}
