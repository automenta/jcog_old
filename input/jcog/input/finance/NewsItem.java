/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input.finance;

import java.io.Serializable;
import org.horrabin.horrorss.RssItemBean;

/**
 *
 * @author seh
 */
public class NewsItem implements Serializable {
    public final String date;
    public final String title;
    public final String desc;
    public final String link;
    //TODO add other fields

    public NewsItem(String date, String title, String desc, String link) {
        this.date = date;
        this.title = title;
        this.desc = desc;
        this.link = link;
    }

    public NewsItem(RssItemBean b) {
        this(b.getPubDate(), b.getTitle(), b.getDescription(), b.getLink());
    }

    @Override
    public String toString() {
        return title + "[" + desc + "]";
    }
    
    
}
