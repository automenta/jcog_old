/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input.finance;

import java.util.Iterator;
import jcog.input.calais.CalaisTag;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TreeSet;
import jcog.input.calais.CalaisEntity;
import jcog.input.calais.CalaisTopic;
import jcog.opencog.AtomType;

/**
 * Collection of TickerPoint's for a given market symbol from a specific date to a specific date
 * @author seh
 */
public class Ticker implements Serializable, AtomType {
    
    public String symbol;
    public final Date from;
    public final Date to;
    
    public final TreeSet<TickerPoint> data = new TreeSet(); //performance hisstory
    public final List<NewsItem> news = new ArrayList();
    
    public final List<CalaisTag> tags = new ArrayList();    
    public final List<CalaisTopic> topics = new ArrayList();
    public final List<CalaisEntity> entities = new ArrayList();

    public Ticker(String symbol, Date from, Date to) {
        this.symbol = symbol;
        this.from = from;
        this.to = to;
    }

    public double getMaxChange(MarketIndicator marketIndicator) {
        Iterator<TickerPoint> i = data.descendingIterator();
        double maxChange = 0;
        TickerPoint last = null;
        while (i.hasNext()) {
            TickerPoint tp = i.next();
            if (last!=null) {
                double d = Math.abs(tp.getData(marketIndicator) - last.getData(marketIndicator));
                if (d > maxChange) maxChange = d;
            }
            last = tp;
        }
        return maxChange;
    }

    public CalaisTag getTag(String name) {
        for (CalaisTag ct : tags) {
            if (ct.name.equals(name))
                return ct;
        }
        return null;
    }

    public CalaisTopic getTopic(String id) {
        for (CalaisTopic ct : topics) {
            if (ct.id.equals(id))
                return ct;
        }
        return null;
    }

    public double getMin(MarketIndicator marketIndicator) {
        double min = data.first().getData(marketIndicator);
        for (TickerPoint tp : data) {
            double v = tp.getData(marketIndicator);
            if (v < min)
                min = v;
        }
        return min;
    }
    public double getMax(MarketIndicator marketIndicator) {
        double max = data.first().getData(marketIndicator);
        for (TickerPoint tp : data) {
            double v = tp.getData(marketIndicator);
            if (v > max)
                max = v;
        }
        return max;
    }
    
}
