/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input.finance;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jcog.input.calais.CalaisEntity;
import jcog.input.calais.CalaisTag;
import jcog.input.calais.CalaisTopic;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.OCMind;

/**
 *
 * @author seh
 */
public class TickerGrapher {
    
    Map<Ticker, Atom> tickerAtoms = new HashMap();
    Map<String, Atom> tagAtoms = new HashMap();
    private final OCMind mind;
    
    public TickerGrapher(OCMind mind, List<Ticker> tickers) {
        super();
        
        this.mind = mind;
        
        for (final Ticker t : tickers) {
            final Atom ta = mind.addVertex(Ticker.class, t.symbol);
            tickerAtoms.put(t, ta);
            
            for (CalaisTag tag : t.tags) {
                Atom e = mind.addEdge(AtomType.intensionalInheritanceLink, tag.name, getTag(tag), ta);
                mind.getTruth(e).setMean(tag.importance);
            }
            for (CalaisEntity ent : t.entities) {
                Atom e = mind.addEdge(AtomType.intensionalInheritanceLink, ent.name, getEntity(ent), ta);
                mind.getTruth(e).setMean(ent.relevance);
                
                Atom f = mind.addEdge(AtomType.intensionalInheritanceLink, ent.type, getEntityType(ent), getEntity(ent));
            }
            for (CalaisTopic top : t.topics) {
                Atom e = mind.addEdge(AtomType.intensionalInheritanceLink, top.name, getTopic(top), ta);
                mind.getTruth(e).setMean(top.score);
            }
        }
    }
    
    public Atom getTag(CalaisTag tag) {
        Atom a = tagAtoms.get(tag.id);
        if (a == null) {
            a = mind.addVertex(CalaisTag.class, tag.name);
            tagAtoms.put(tag.id, a);
        }
        return a;
    }

    public Atom getTopic(CalaisTopic top) {
        Atom a = tagAtoms.get(top.id);
        if (a == null) {
            a = mind.addVertex(CalaisTopic.class, top.name);
            tagAtoms.put(top.id, a);
        }
        return a;
    }

    public Atom getEntity(CalaisEntity ent) {
        Atom a = tagAtoms.get(ent.id);
        if (a == null) {
            a = mind.addVertex(CalaisEntity.class, ent.name);
            tagAtoms.put(ent.id, a);
        }
        return a;
    }
    
    public Atom getEntityType(CalaisEntity ent) {
        //TODO use a separate CalaisEntityType.class
        
        final String i = "eType:" + ent.type;
        Atom a = tagAtoms.get(i);
        if (a == null) { 
            a = mind.addVertex(CalaisEntity/*Type*/.class, ent.type);
            tagAtoms.put(i, a);
        }
        return a;
    }
    
}
