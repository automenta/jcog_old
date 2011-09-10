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
import edu.uci.ics.jung.algorithms.layout.FRLayout;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.visualization.BasicVisualizationServer;
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position;
import java.awt.Dimension;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
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
public class TestDynamicBayes {
    
    
    public static class TickerBayesianNetwork {
        public final static Logger logger = Logger.getLogger(TickerBayesianNetwork.class.toString());
        
        public final MutableBayesianAdjacencyNetwork network = new MutableBayesianAdjacencyNetwork();
        private final int changeResolution;
        private final int historyLength;
        private final Ticker ticker;
        
        private List<GraphicalModelNode<Integer>> past; //0 = previous change, 1 = previous previous, ...
        private final double maxValueChange;
        
        public TickerBayesianNetwork(Ticker c, int changeResolution, int historyLength) {
            super();
            
            this.ticker = c;
            
            this.changeResolution = changeResolution;
            this.historyLength = historyLength;
            
            past = new ArrayList(historyLength);
            
            GraphicalModelNode<Integer> futureState = newPriceState("Future");
            network.add(futureState);
            
            for (int i = 0; i < historyLength; i++) {
                GraphicalModelNode<Integer> ps = newPriceState("Past-" + i);                
                past.add(ps);
                network.add(ps);                
            }
            for (int i = 0; i < historyLength; i++) {
                
                for (int j = 1; j <= historyLength; j++) {
                    if (i - j >= 0) {
                        //System.out.println((i-j) + " <- " + i );
                        network.add(new ImmutableDirectedEdge<GraphicalModelNode>(past.get(i), past.get(i-j)));
                    }
                }
                    
                //System.out.println("future <- " + i );
                network.add(new ImmutableDirectedEdge<GraphicalModelNode>(past.get(i), futureState));
            }
            
            //System.out.println("edges: " + network.getEdges().size());
            
            maxValueChange = c.getMaxChange(MarketIndicator.CLOSE);
            System.out.println("max daily change in market close = " + maxValueChange);
            
            List<TickerPoint> tpl = new ArrayList(c.data);
            if (tpl.size() <= historyLength ) {
                logger.severe("history of " + c + " not long enough for historyLength=" + historyLength );
                return;
            }
            
            for (int i = historyLength+1; i < tpl.size(); i++) {
               final TickerPoint now = tpl.get(i);
               
               double d = now.getData(MarketIndicator.CLOSE) - tpl.get(i-1).getData(MarketIndicator.CLOSE);
               futureState.setState(getChangeState(d));

               System.out.println(tpl.get(i).getWhen() + " " + d + " " + getChangeState(d));
                               
               for (int j = 1; j <= historyLength; j++) {
                   System.out.println("  " + tpl.get(i-j-1).getWhen() + " to " + tpl.get(i-j).getWhen() + " into history " + (j-1) );
                   double e = tpl.get(i-j).getData(MarketIndicator.CLOSE) - tpl.get(i-j-1).getData(MarketIndicator.CLOSE);
                   past.get(j-1).setState(getChangeState(e));
               }
               
               network.learnStates();
            }
            
            Set<GraphicalModelNode> goals = new HashSet();
            goals.add(futureState);
            
            Set<GraphicalModelNode> influences = new HashSet();
            influences.add(past.get(0));
            
            System.out.println("PREDICTION");
            for (int i = 1; i < past.size(); i++) {
                past.get(i-1).setState(past.get(i).getState());
            }
            past.get(past.size()-1).setState(futureState.getState());
            
            for (int i = 0; i < past.size(); i++)
                System.out.print(past.get(i).getState() + " ");
            System.out.println();
                        
            for (int i = -changeResolution; i <= changeResolution; i++) {
                futureState.setState(i);
                
                double p = network.conditionalProbability(goals, influences);
                
                System.out.println(i + "=" + p);                
            }
        }

        /** discretizes the price change */
        public int getChangeState(double change) {
            double prop = change / maxValueChange;
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
        
        Layout layout = new FRLayout(jg, new Dimension(400,400));
        layout.setSize(new Dimension(400,400)); // sets the initial size of the space
        
        BasicVisualizationServer<Integer,String> vv = new BasicVisualizationServer<Integer,String>(layout);
        
        vv.getRenderContext().setVertexLabelTransformer(new ToStringLabeller());
        //vv.getRenderContext().setEdgeLabelTransformer(new ToStringLabeller());
        vv.getRenderer().getVertexLabelRenderer().setPosition(Position.CNTR);
        vv.setPreferredSize(new Dimension(350,350)); //Sets the viewing area size
        
        JFrame frame = new JFrame("Simple Graph View");
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
            Ticker c = ts.load("IBM", startDate, endDate);
            
            
            TickerBayesianNetwork tbn = new TickerBayesianNetwork(c, 8, 3);
            displayGraph(tbn.network);
            
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        
    }
}
