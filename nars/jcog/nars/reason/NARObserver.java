/**
 * 
 */
package jcog.nars.reason;

/** implementers of NARObserver will be notified of certain NARS events */
public interface NARObserver {
	public void onCycle(NAR nar);
}