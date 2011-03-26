package jcog.opencog.attention;

import java.util.ArrayList;
import java.util.List;
import jcog.opencog.Atom;
import jcog.opencog.AtomTypes;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;


/** Spreads short term importance along HebbianLinks.
 *
 * Currently only spreads along Symmetric and Inverse HebbianLinks.
 *
 * @todo Spread along asymmetric hebbian links too.
 * @todo Spread along symmetric inverse hebbian links too.
 * @todo Optionally spread long term importance.
 */
public class SpreadImportance extends MindAgent {


	short spreadThreshold;
	short stealingLimit;
	float importanceSpreadingMultiplier;
	//RecentLong amountSpread;

        @Override public void run(OCMind mind) {
	
            //TODO this is a hack to just get some spreading activity
            for (Atom e : mind.getAtoms(AtomTypes.SymmetricHebbianLink, false)) {
                List<Atom> linked = new ArrayList(mind.getIncidentVertices(e));
                if (linked.size() == 2) {
                    Atom a = linked.get(0);
                    Atom b = linked.get(1);
                    
                    short aSTI = mind.getSTI(a);
                    short bSTI = mind.getSTI(b);
                    short absDifference = (short)Math.abs(aSTI - bSTI);
                    if (absDifference >= 2) {
                        short rate = 1;
                        if (aSTI > bSTI) {
                            mind.getAttention(a).addSTI((short)-rate);
                            mind.getAttention(b).addSTI(rate);
                        }
                        else if (bSTI > aSTI) {
                            mind.getAttention(b).addSTI((short)-rate);
                            mind.getAttention(a).addSTI(rate);
                        }
                    }
                }
            }
                    
	}

	public SpreadImportance() {
		super();
	}

	/** Spread importance for an atom.
	 *
	 * @param h The handle for the atom to spread importance for.
	 */
	protected void spreadAtomImportance(Atom a) {

	}

	/** Spread importance along Hebbian links. */
	protected void spreadImportance() {

	}

    //! Sum total difference for an atom
    protected int sumTotalDifference(Atom source, List<Atom> links) {
    	//TODO
    	return 0;    	
    }

    //! Sum difference for one link
    protected int sumDifference(Atom source, Atom link) {
    	//TODO
    	return 0;
    }

    //! Calculate the difference for an inverse link
    protected float calcInverseDifference(short s, short t, float weight) {
    	//TODO
    	return 0;    	
    }

    //! Calculate the difference for a normal Hebbian link
    protected float calcDifference(short s, short t, float weight) {
    	//TODO
    	return 0;
    }


	/** Set minimal amount of STI necessary for an atom to have before it
	 * spreads STI.
	 *
	 * @param t the threshold.
	 */
	void setSpreadThreshold(short t) {
		spreadThreshold = t;
	}


	/** Set the multiplier for converting the HebbianLink TruthValue to STI.
	 *
	 * If multiplier is zero, then \b all STI above the threshold is evenly
	 * distributed.
	 *
	 * @param m the multiplier.
	 */
	void setImportanceSpreadingMultiplier(float m) {
		importanceSpreadingMultiplier = m;
	}

}
