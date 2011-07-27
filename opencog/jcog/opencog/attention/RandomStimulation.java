/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.attention;

import java.util.List;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;

/**
 *
 * @author seh
 */
public class RandomStimulation extends MindAgent {

    private short boost;

    public RandomStimulation(double period, short boost) {
        super(period);
        this.boost = boost;
    }

    public short getBoost() {
        return boost;
    }

    public void setBoost(short b) {
        this.boost = b;
    }

    @Override
    protected void run(OCMind mind) {
        List<Atom> c = mind.getAtoms(AtomType.conceptNode, false);
        if (c != null) {
            if (c.size() > 0) {
                int i = RandomNumber.getInt(0, c.size() - 1);
                Atom x = c.get(i);
                addStimulus(x, boost);
            }
        }
    }
}
