/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.attention;

import java.util.List;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import org.apache.commons.collections15.IteratorUtils;

/**
 *
 * @author seh
 */
public class RandomStimulation extends MindAgent {

    private short boost;
    private int numAtoms;

    public RandomStimulation(double period, short boost, int numAtoms) {
        super(period);
        this.boost = boost;
        this.numAtoms = numAtoms;
    }

    public short getBoost() {
        return boost;
    }

    public void setBoost(short b) {
        this.boost = b;
    }

    public int getNumAtoms() {
        return numAtoms;
    }

    public void setNumAtoms(int numAtoms) {
        this.numAtoms = numAtoms;
    }

    @Override
    protected void run(OCMind mind) {
        //TODO avoid having to reinstantiate arraylist each time this is called
        List<Atom> c = IteratorUtils.toList(mind.iterateAtoms());
        
        if (c != null) {
            if (c.size() > 0) {
                for (int n = 0; n < numAtoms; n++) {
                    int i = RandomNumber.getInt(0, c.size() - 1);
                    Atom x = c.get(i);
                    addStimulus(x, getBoost());
                }
            }
        }
    }
}
