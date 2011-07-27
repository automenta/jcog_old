package jcog.opencog;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
 * 
 * This may be a good idea, re: UpdateImportance:
        In the MindAgent base class, provide a method rewardAtoms() which calls the WageAgent. This method would also signal to the WageAgent who the caller was so that the correct stimulus map and amount of STI for reward could be worked out.
 */
public abstract class MindAgent {

    private String id;
    private double period = 0.0;
    double accumulatedDT = 0.0;
    
    final private Map<Atom, Short> atomStimulus = new HashMap();
    //private List<Atom> utilizedAtoms = new LinkedList();
    final private Set<Atom> verticesToRemove = new HashSet(), edgesToRemove = new HashSet();
    
    /**
     * Initializes a MindAgent with name set by the simple name of the class.
     * Only use this constructor if you want to create a default or singleton MindAgent for the class
     * being implemented, since all MindAgents need to have unique names.
     */
    public MindAgent(double period) {
        this(period, null);
        this.id = getClass().getSimpleName();
    }

    public MindAgent() {
        this(0);        
    }
    
    public MindAgent(double period, String id) {
        super();
        this.id = id;
    }

    /** determines how often this agent is scheduled to run().  1 means will be executed each cycle, 2 means every other cycle, etc. (was 'frequency' parameter) */
    public double getPeriod() {
        return period;
    }

    /**
     * 
     * @param period for calls to run().  if < 0, it disables the mindagent from running
     */
    public void setPeriod(double period) {
        this.period = period;
    }
    
    public void _run(OCMind mind, double dt) {
        accumulatedDT += dt;
        
        if (getPeriod() < 0)
            return;
        
        if (accumulatedDT >= getPeriod()) {
            run(mind);
            accumulatedDT = 0;
        }
    }
    
    public void removeVertex(Atom a) {
        verticesToRemove.add(a);
    }
    public void removeEdge(Atom a) {
        edgesToRemove.add(a);
    }
    
    /** the activity that will be invoked when the mind schedules it to run */
    abstract protected void run(OCMind mind);

//	/** The atoms utilized by the Agent in a single cycle, to be used by the
//	 *  System Activity Table to assign credit to this agent. */
//	public List<String> getUtilizedAtoms() {
//		return utilizedAtoms;
//	}
//
//	protected void resetUtilizedAtoms() { 
//		utilizedAtoms.clear();
//	}

    /**
     * Get total stimulus.
     *
     * @return total stimulus since last reset.
     */
    public short getTotalStimulus() {
        short sum = 0;
        for (Short s : atomStimulus.values())
            sum += s.shortValue();
        return sum;
    }

    /**
     * Stimulate a Handle's atom.
     *
     * @param amount of stimulus to give.
     * @return total stimulus given since last reset.
     */
    public short addStimulus(Atom a, short deltaAmount) {
        short newStimulus = (short)(getStimulus(a) + deltaAmount);
        setStimulus(a, newStimulus);
        return newStimulus; 
    }

    public void setStimulus(Atom a, short amount) {
        atomStimulus.put(a, amount);
    }

    //    /**
    //     * Stimulate all atoms in HandleEntry list.
    //     *
    //     * @param linked list of atoms to spread stimulus across.
    //     * @param amount of stimulus to share.
    //     * @return remainder stimulus after equal spread between atoms.
    //     */
    //    stim_t stimulateAtom(HandleEntry* h, stim_t amount);

    public void clearStimulus() {
        atomStimulus.clear();
    }

    /**
     * Reset stimulus.  Sets stimulus to zero but doesn't remove entries from the stimulus Map
     *
     * ?? @return new stimulus since reset, usually zero unless another
     * thread adds more.
     */
    public void resetStimulus() {
        List<Atom> atoms = new LinkedList(atomStimulus.keySet());
        for (Atom a : atoms) {
            atomStimulus.put(a, (short)0);
        }        
    }
    
    

    public short getStimulus(Atom a) {
        Short s = atomStimulus.get(a);
        if (s == null)
            return 0;
        return s.shortValue();
    }

    @Override
    public int hashCode() {
        return id.hashCode();
    }
    
    @Override
    public String toString() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (o.getClass().equals(getClass())) {
           MindAgent m = (MindAgent)o;
           return m.id.contains(id);
        }
        return false;
    }

    public Collection<Atom> getStimulated() {
        return atomStimulus.keySet();
    }

    public Collection<Atom> getVerticesToRemove() {
        return verticesToRemove;
    }
    
    public Collection<Atom> getEdgesToRemove() {
        return edgesToRemove;
    }        
    
}
