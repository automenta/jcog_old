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
    
}