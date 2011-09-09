/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input.finance;

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author seh
 */
public class TickerPoint implements Comparable<TickerPoint>, Serializable {
    public final int INDEX_DOUBLE_HIGH = 0;
    public final int INDEX_DOUBLE_LOW = 0;
    public final int INDEX_DOUBLE_OPEN = 0;
    public final int INDEX_DOUBLE_CLOSE = 0;
    /**
     * When was this data sample taken.
     */
    private final Date when;
    /**
     * What is the ticker symbol for this data sample.
     */
    private final String tickerSymbol;
    /**
     * The data that was collection for the sample date.
     */
    private final Map<TickerSource.MarketDataType, Double> data;

    @Override
    public String toString() {
        return tickerSymbol + "@" + when + "=" + data;
    }

    /**
     * The logging object.
     */
    //        @SuppressWarnings("unused")
    //        private final Logger logger = LoggerFactory.getLogger(this.getClass());
    /**
     * Construct one sample of market data.
     *
     * @param when
     *            When was this sample taken.
     * @param ticker
     *            What is the ticker symbol for this data.
     */
    public TickerPoint(final Date when, final String tickerSymbol) {
        this.when = when;
        this.tickerSymbol = tickerSymbol;
        this.data = new HashMap<TickerSource.MarketDataType, Double>();
    }

    /**
     * {@inheritDoc}
     */
    public int compareTo(final TickerPoint other) {
        return getWhen().compareTo(other.getWhen());
    }

    /**
     * Get one type of market data from this date.
     *
     * @param type
     *            The type of data needed.
     * @return The market data for the specified date and of the specified type.
     */
    public double getData(final TickerSource.MarketDataType type) {
        return this.data.get(type);
    }

    /**
     * @return The ticker symbol this sample is assocated with.
     */
    public String getTicker() {
        return this.tickerSymbol;
    }

    /**
     * @return When this sample was taken.
     */
    public Date getWhen() {
        return this.when;
    }

    /**
     * Set financial data for this date.
     *
     * @param type
     *            The type of data being set.
     * @param data
     *            The value of the data being set.
     */
    public void setData(final TickerSource.MarketDataType type, final double data) {
        this.data.put(type, data);
    }
    
}
/**
 * The logging object.
 */
