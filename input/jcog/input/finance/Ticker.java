/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input.finance;

import jcog.input.calais.CalaisTag;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import jcog.input.calais.CalaisEntity;
import jcog.input.calais.CalaisTopic;
import jcog.opencog.AtomType;

/**
 * Collection of TickerPoint's for a given market symbol from a specific date to a specific date
 * @author seh
 */
public class Ticker implements Serializable, AtomType {
    
    public final String symbol;
    public final Date from;
    public final Date to;
    
    public final List<TickerPoint> data = new ArrayList(); //performance hidstory
    public final List<NewsItem> news = new ArrayList();
    
    public final List<CalaisTag> tags = new ArrayList();    
    public final List<CalaisTopic> topics = new ArrayList();
    public final List<CalaisEntity> entities = new ArrayList();

    public Ticker(String symbol, Date from, Date to) {
        this.symbol = symbol;
        this.from = from;
        this.to = to;
    }
    
}
