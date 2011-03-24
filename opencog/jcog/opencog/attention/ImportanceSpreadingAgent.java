package jcog.opencog.attention;

import java.util.List;
import jcog.opencog.Atom;
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
public class ImportanceSpreadingAgent extends MindAgent {


	short spreadThreshold;
	short stealingLimit;
	float importanceSpreadingMultiplier;
	//RecentLong amountSpread;

	public ImportanceSpreadingAgent() {
		super(ImportanceSpreadingAgent.class.getSimpleName());
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

	@Override public void run(OCMind mind) {
		//TODO
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
