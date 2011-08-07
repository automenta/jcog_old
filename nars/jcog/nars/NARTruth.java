package jcog.nars;

import jcog.nars.reason.io.Symbols;



public class NARTruth {
    /** The charactor that marks the two ends of a truth value */
    private static final char DELIMITER = Symbols.TRUTH_VALUE_MARK;
    /** The charactor that separates the factors in a truth value */
    private static final char SEPARATOR = Symbols.VALUE_SEPARATOR;

	//Note: in Open-NARS, truth values are stored as "ShortFloat" - To save space, the values are stored as short integers (-32768 to 32767, only 0 to 10000 used),  but used as float

	protected float frequency;
	protected float confidence;

    public NARTruth() {
    	this(0, 0);
    }
    
    public NARTruth(float frequency, float confidence) {
    	super();
    	this.frequency = frequency;
    	this.confidence = confidence;
    }
    
	
	public float getFrequency() {
		return frequency;
	}
	
	public float getConfidence() {
		return confidence;
	}

	
    /** 
     * Calculate the expectation value of the truth value
     * @return The expectation value
     */
    public float getExpectation() {
        return (float) (getConfidence() * (getFrequency() - 0.5) + 0.5);
    }

    /** 
     * Calculate the absolute difference of the expectation value and that of a given truth value
     * @param t The given value
     * @return The absolute difference
     */
    public float getExpDifAbs(NARTruth t) {
        return Math.abs(getExpectation() - t.getExpectation());
    }

    /**
     * Compare two truth values
     * @param that The other TruthValue
     * @return Whether the two are equivalent
     */
    @Override public boolean equals(Object that) {
        return ((that instanceof NARTruth) 
                && (getFrequency() == ((NARTruth) that).getFrequency()) 
                && (getConfidence() == ((NARTruth) that).getConfidence()));
    }

    /**
     * The hash code of a TruthValue
     * @return The hash code
     */
    @Override
    public int hashCode() {
        int hash = 5;
        return hash;
    }

    /**
     * The String representation of a TruthValue
     * @return The String
     */
    @Override public String toString() {
        return DELIMITER + Float.toString(getFrequency()) + SEPARATOR + Float.toString(getConfidence()) + DELIMITER;
    }

//    /**
//     * A simplified String representation of a TruthValue, where each factor is accruate to 1%
//     * @return The String
//     */
    public String toString2() {
        return DELIMITER + frequency + SEPARATOR + Float.toString(confidence) + DELIMITER;
    }

}
