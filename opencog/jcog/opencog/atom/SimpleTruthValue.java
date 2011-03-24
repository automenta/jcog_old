/**
 * 
 */
package jcog.opencog.atom;

public class SimpleTruthValue implements TruthValue {

    private double mean;
    private long count;
    private double confidence;

    public SimpleTruthValue() {
        super();
        mean = 0;
        count = 0;
        confidence = 0;
    }

    public double getMean() {
        return mean;
    }

    public long getCount() {
        return count;
    }

    public double getConfidence() {
        return confidence;
    }
}