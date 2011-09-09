/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input.finance;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.Calendar;
import java.util.Date;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;
import jcog.input.calais.Calais;
import jcog.input.calais.CalaisEntity;
import jcog.input.calais.CalaisTag;
import jcog.input.calais.CalaisTopic;
import mx.bigdata.jcalais.CalaisObject;
import mx.bigdata.jcalais.CalaisResponse;
import org.encog.util.csv.CSVFormat;
import org.encog.util.csv.ReadCSV;
import org.encog.util.http.FormUtility;
import org.horrabin.horrorss.RssChannelBean;
import org.horrabin.horrorss.RssImageBean;
import org.horrabin.horrorss.RssItemBean;
import org.horrabin.horrorss.RssParser;

/**
 * Gathers data from various webservices given a ticker symbol
 *
 * @author jheaton
 * @author SeH
 */
public class TickerSource  {
    public static final Logger logger = Logger.getLogger(TickerSource.class.toString());

    private ConcurrentHashMap<URL,Ticker> pageCache;
    private final String cachePath;
    
    public TickerSource(String cachePath) {
        super();
        
        this.cachePath = cachePath;
        
        try {
            ObjectInputStream ois = new ObjectInputStream(new FileInputStream(cachePath));
            pageCache = (ConcurrentHashMap)ois.readObject();
            ois.close();
        } catch (Exception ex) {
            pageCache = new ConcurrentHashMap();
        }
        
        Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    save();
                } catch (IOException ex) {
                    logger.severe(ex.toString());
                }
            }            
        }));
        
    }
    
    public synchronized void save() throws IOException {
        ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(cachePath));
        oos.writeObject(pageCache);
        oos.close();        
    }
    
    public enum MarketDataType {

        /**
         * The market open for the day.
         */
        OPEN,
        /**
         * The market close for the day.
         */
        CLOSE,
        /**
         * The volume for the day.
         */
        VOLUME,
        /**
         * The adjusted close. Adjusted for splits and dividends.
         */
        ADJUSTED_CLOSE,
        /**
         * The high for the day.
         */
        HIGH,
        /**
         * The low for the day.
         */
        LOW
    }

    /**
     * This method builds a URL to load data from Yahoo Finance for a neural
     * network to train with.
     *
     * @param ticker
     *            The ticker symbol to access.
     * @param from
     *            The beginning date.
     * @param to
     *            The ending date.
     * @return The UEL
     * @throws IOException
     *             An error accessing the data.
     */
    private URL buildURL(final String tickerSymbol, final Date from, final Date to) throws IOException {
        // process the dates
        final Calendar calendarFrom = Calendar.getInstance();
        calendarFrom.setTime(from);
        final Calendar calendarTo = Calendar.getInstance();
        calendarTo.setTime(to);

        // construct the URL
        final OutputStream os = new ByteArrayOutputStream();
        final FormUtility form = new FormUtility(os, null);
        form.add("s", tickerSymbol.toUpperCase());
        form.add("a", "" + calendarFrom.get(Calendar.MONTH));
        form.add("b", "" + calendarFrom.get(Calendar.DAY_OF_MONTH));
        form.add("c", "" + calendarFrom.get(Calendar.YEAR));
        form.add("d", "" + calendarTo.get(Calendar.MONTH));
        form.add("e", "" + calendarTo.get(Calendar.DAY_OF_MONTH));
        form.add("f", "" + calendarTo.get(Calendar.YEAR));
        form.add("g", "d");
        form.add("ignore", ".csv");
        os.close();
        final String str = "http://ichart.finance.yahoo.com/table.csv?"
                + os.toString();
        return new URL(str);
    }
    
    /**
     * Load the specified financial data.
     *
     * @param ticker
     *            The ticker symbol to load.
     * @param dataNeeded
     *            The financial data needed.
     * @param from
     *            The beginning date to load data from.
     * @param to
     *            The ending date to load data to.
     * @return A collection of LoadedMarketData objects that represent the data
     *         loaded.
     */
    public Ticker load(final String tickerSymbol, final Date from, final Date to) throws IOException {
                    
            final URL url = buildURL(tickerSymbol, from, to);
            
            Ticker result = pageCache.get(url);
            
            if (result == null) {
                final InputStream is = url.openStream();
                logger.info("Downloading " + url.toString());
                final ReadCSV csv = new ReadCSV(is, true, CSVFormat.ENGLISH);
                
                result = new Ticker(tickerSymbol, from, to);
                
                while (csv.next()) {
                    final Date date = csv.getDate("date");
                    final double adjClose = csv.getDouble("adj close");
                    final double open = csv.getDouble("open");
                    final double close = csv.getDouble("close");
                    final double high = csv.getDouble("high");
                    final double low = csv.getDouble("low");
                    final double volume = csv.getDouble("volume");

                    final TickerPoint data = new TickerPoint(date, tickerSymbol);
                    data.setData(MarketDataType.ADJUSTED_CLOSE, adjClose);
                    data.setData(MarketDataType.OPEN, open);
                    data.setData(MarketDataType.CLOSE, close);
                    data.setData(MarketDataType.HIGH, high);
                    data.setData(MarketDataType.LOW, low);
                    data.setData(MarketDataType.OPEN, open);
                    data.setData(MarketDataType.VOLUME, volume);
                    
                    result.data.add(data);
                }
                
                loadNews(result);
                loadCalais(result);
                
                pageCache.put(url, result);

                csv.close();
                is.close();

            }
            else {
                logger.info("Cached: " + url.toString());
            }

            return result;
    }
    
    protected void loadCalais(Ticker h) {
        StringBuilder allTitles = new StringBuilder();
        for (NewsItem ni : h.news) {
            allTitles.append(ni.title);
            allTitles.append(". ");
        }
        
        CalaisResponse response;
        try {
            response = Calais.getCalais(allTitles.toString());
            for (CalaisObject entity : response.getEntities()) {
                h.entities.add(new CalaisEntity(entity));
            }
            for (CalaisObject topic : response.getTopics()) {
                h.topics.add(new CalaisTopic(topic));
            }
            for (CalaisObject tags : response.getSocialTags()){
                h.tags.add(new CalaisTag(tags));
            }
        } catch (Exception ex) {
            logger.severe(ex.toString());
        }
    }
    
    protected void loadNews(Ticker h) {
        loadNewsItems(h, "http://finance.yahoo.com/rss/headline?s=" + h.symbol);
        loadNewsItems(h, "http://finance.yahoo.com/rss/industry?s=" + h.symbol);
    }
    
    protected void loadNewsItems(Ticker h, String url) {
        
        RssParser rss = new RssParser(url);
        try {
            rss.parse();
            RssChannelBean channel = rss.getChannel(); //Obtain the channel element
            RssImageBean image = rss.getImage(); //Obtain the image element
            Vector items = rss.getItems(); //Obtain a Vector of item elements (RssItemBean)

            // Iterate the list items
            for (int i = 0; i < items.size(); i++) {
                RssItemBean item = (RssItemBean) items.elementAt(i); //Cast the Object from the list to RssItemBean
//                System.out.println("Title: " + item.getTitle());
//                System.out.println("Link : " + item.getLink());
//                System.out.println("Desc.: " + item.getDescription());
                h.news.add(new NewsItem(item));
            }


        } catch (Exception e) {
            logger.severe(e.toString());
        }
        
    }
    
}