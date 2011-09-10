/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input;

import java.io.IOException;
import java.util.Date;
import jcog.input.finance.Ticker;
import jcog.input.finance.TickerSource;
import org.encog.util.time.DateUtil;

/**
 *
 * @author seh
 */
public class TestTicker {
    public final static String cachePath = "/tmp/" + TestTicker.class.getSimpleName() + ".cache";
    public final static TickerSource tickerSource = new TickerSource(cachePath);
    
    public static TickerSource getTickerSource() {
        return tickerSource;
    }
    
    public static void main(String[] args) {
        Date startDate = DateUtil.createDate(1, 4, 2011);
        
                
        try {
            Ticker c = getTickerSource().load("IBM", startDate, new Date());
            System.out.println(c.data);
            System.out.println(c.news);
            System.out.println(c.topics);
            System.out.println(c.tags);
            System.out.println(c.entities);
            
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
