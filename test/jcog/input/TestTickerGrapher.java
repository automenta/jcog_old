/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input;

import java.io.IOException;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import jcog.graphstream.GraphStream;
import jcog.input.finance.Ticker;
import jcog.input.finance.TickerGrapher;
import jcog.input.finance.TickerSource;
import jcog.opencog.OCMind;
import org.encog.util.time.DateUtil;

/**
 *
 * @author seh
 */
public class TestTickerGrapher {

    public static void main(String[] args) {
        Date startDate = DateUtil.createDate(1, 4, 2011);
        Date today = new Date();
        
        String cachePath = "/tmp/" + TestTicker.class.getSimpleName() + ".cache";
        
        List<Ticker> tickers = new LinkedList();
        
        String[] symbols = new String[] { "IBM", "AAPL", "MSFT", "GOOG", "INTC", "AMD", "NTDOY", "SNE", "ATVI", "^NYA" };
        final TickerSource source = new TickerSource(cachePath);
        

        try {
            for (String s : symbols) {
                tickers.add(source.load(s, startDate, today));
            }
            
            OCMind m = new OCMind();
            
            new TickerGrapher(m, tickers);
            
            m.printAtoms();
            
            new GraphStream(m, 8000);
            
        } catch (Exception ex) {
            ex.printStackTrace();
        }

    }
}
