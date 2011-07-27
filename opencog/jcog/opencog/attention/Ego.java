/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.attention;

import java.util.Iterator;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import org.apache.commons.collections15.Predicate;

/**
 * amplifies already-high STI concepts and iteratively speaks them. implements a markov-chain weighted by STI
 */
/**
 *
 * @author seh
 */
public abstract class Ego extends MindAgent {
    //        int historySize = 4;
    //        Queue<Atom> history = new ArrayDeque<Atom>(historySize);
    short egoFocusBoost = 0;
    Atom last = null;

    public Ego(double period) {
        setPeriod(period);
    }

    @Override
    protected void run(final OCMind mind) {
        Iterator<Atom> ci = mind.iterateAtomsByDecreasingSTI(new Predicate<Atom>() {
            @Override
            public boolean evaluate(Atom x) {
                return (mind.getType(x).equals(AtomType.conceptNode));
            }            
        });
        while (ci.hasNext()) {
            Atom leading = ci.next();
            if (speakable(leading)) {
                speak(leading);
                last = leading;
                break;
            }
        }
    }    
    
    //public Map<Atom,Double> getNextChoices();

    public abstract boolean speakable(Atom l);

    //emit, utter, speak, transmit
    public abstract void speak(Atom a);
    
}
