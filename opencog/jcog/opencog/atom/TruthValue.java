/**
 * 
 */
package jcog.opencog.atom;

/** Truth Value 
 * 	@see http://opencog.org/wiki/TruthValue
 */
public interface TruthValue {

    public double getMean();

    public long getCount();

    public double getConfidence();

    public void setConfidence(double confidence);

    public void setMean(double mean);

    public void setCount(long count);
    

}