package jcog.opencog;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;


/**
 * Typically, an opencog agent is coded similarly to a Java thread (except that
 * there's currently no concurrency between multiple agents): it has a "run"
 * method which is called by the server everytime the agent is selected for
 * execution. So, to write a custom agent, all one has to do is to derive from
 * the Agent class and implement the desired behavior inside the 'run' method.
 *
 * The Agent base class also provides the 'frequency' attribute which
 * defines how often the agent will be executed. A value of 1 (the default)
 * means that the agent will be executed every server cycle. A value of 2 means
 * that the agent will be executed every 2 cycles. And so on.
 * 
 * Since agents are registered with the cogserver using the Registry+Factory
 * pattern, agent classes must override the 'classinfo' which uniquelly
 * identifies its class. Typicall the 'classinfo' method will simply forward to
 * a call to an 'info' class method that provides the actual class info -- the
 * 'info' class method is required by the Registry+Factory anyway.
 */
public abstract class MindAgent  {

	private int period;
	private short totalStimulus;
	private Map<String, Short> atomStimulus;
	private List<String> utilizedAtoms = new LinkedList();
	private String id;

	public MindAgent(String id) {
		super();
		
		this.id = id;

//		this.attention = new OCPAttention();
//		set(OCPAttention.class, attention);
	}

	/** determines how often this agent is scheduled to run().  1 means will be executed each cycle, 2 means every other cycle, etc. (was 'frequency' parameter) */ 
	public int getPeriod() {
		return period;
	}

	/** the activity that will be invoked when the mind schedules it to run */
	abstract public void run(OCMind mind);

	/** The atoms utilized by the Agent in a single cycle, to be used by the
	 *  System Activity Table to assign credit to this agent. */
	public List<String> getUtilizedAtoms() {
		return utilizedAtoms;
	}

	protected void resetUtilizedAtoms() { 
		utilizedAtoms.clear();
	}

    /**
     * Get total stimulus.
     *
     * @return total stimulus since last reset.
     */
	public short getTotalStimulus() {
		return totalStimulus;
	}


	/**
	 * Stimulate a Handle's atom.
	 *
	 * @param amount of stimulus to give.
	 * @return total stimulus given since last reset.
	 */
	protected short stimulateAtom(String a, short amount) {
		return 0;
	}

	/**
	 * @return true if removed
	 */
	protected boolean removeAtomStimulus(String a) {
		return false;
	}
	
	//    /**
	//     * Stimulate all atoms in HandleEntry list.
	//     *
	//     * @param linked list of atoms to spread stimulus across.
	//     * @param amount of stimulus to share.
	//     * @return remainder stimulus after equal spread between atoms.
	//     */
	//    stim_t stimulateAtom(HandleEntry* h, stim_t amount);

	/**
	 * Reset stimulus.
	 *
	 * @return new stimulus since reset, usually zero unless another
	 * thread adds more.
	 */
	public void resetStimulus() {
		
	}

	protected short getAtomStimulus(String a) {
		return 0;
	}

}
