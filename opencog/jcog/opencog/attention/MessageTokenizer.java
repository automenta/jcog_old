/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.attention;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;

/**
 *
 * @author seh
 */
public class MessageTokenizer extends MindAgent {

    //TODO remember previous names and their tokenizations so that this can update atoms whose names have changed
    
    boolean addHebbianBetweenTokens = true;
    
    Set<Atom> messagesTokenized = new HashSet();
    Map<String, Atom> words = new HashMap();
    
    public MessageTokenizer(double period /*, int numMessagesPerCycle*/) {
        super(period);
    }

    
    public Atom getWord(OCMind mind, String t) {        
        Atom e = words.get(t.toLowerCase());
        if (e == null) {
            e = mind.addVertex(AtomType.conceptNode, t);
            words.put(t, e);
        }
        return e;
    }
    
    @Override
    protected void run(OCMind mind) {
        //TODO iterate by importance
        Collection<Atom> ia = mind.getAtoms(AtomType.conceptNode, true);
        
        for (Atom a : ia) {
            if (messagesTokenized.contains(a))
                continue;
            
            messagesTokenized.add(a);
            
            String name = mind.getName(a);
                StringTokenizer st = new StringTokenizer(name, " .?!,:;-=/\\|&");
                
                List<Atom> tokens = new LinkedList();
                while (st.hasMoreTokens()) {
                    String t = st.nextToken().trim();
                    tokens.add(getWord(mind, t));
                }
                
                if (tokens.size() > 1) {                
                    mind.addEdge(AtomType.extensionalInheritanceLink, tokens);
                }
                if (addHebbianBetweenTokens) {
                    for (int i = 0; i < tokens.size() -1 ; i++) {
                        mind.addEdge(AtomType.symmetricHebbianLink, tokens.get(i), tokens.get(i+1));
                    }
                }
                
                //TODO process multiple per cycle
                break;
        }
    }
    
}
