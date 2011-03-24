package jcog.nars.reason;

import java.io.Serializable;

/** NAR reasoner parameter object */
 
abstract public class NARParams implements Serializable {

	//TODO make class this an interface
	
	
    /* ---------- initial values of run-time adjustable parameters ---------- */
    
    /** Silent threshold for task reporting, in [0, 100]. */
    public static final int SILENT_LEVEL = 1; // spontaneous report

    /* ---------- time management ---------- */
    /** Maximum TermLinks checked for novelity for each TaskLink in TermLinkBag */
    public static final int MAX_MATCHED_TERM_LINK = 10;
    

    /* ---------- logical parameters ---------- */
    /** Horizon, the amount of evidence coming in the near future. */
    public static final int NEAR_FUTURE = 1;    // or 2, can be float
    
    
    /** Range of temporal induction in input events. */
    public static final int MAXMUM_EVENTS_LENGTH = 10;

    
    


    
    
    /* ---------- avoiding repeated reasoning ---------- */
    /** Maximum length of Stamp, a power of 2 */
    public static final int MAXMUM_STAMP_LENGTH = 16;
    
    
    public int TERM_LINK_RECORD_LENGTH = 10;    
    /** Remember recently used TermLink on a Task */
    public int getTermLinkRecordLength() {
    	return TERM_LINK_RECORD_LENGTH;
    }


    protected float DECISION_THRESHOLD = (float) 0.66;
    /** The desirability threshold for an operation to be executed. */
	public float getDecisionThreshold() {
		return DECISION_THRESHOLD;
	}

    protected float PRIORITY_THRESHOLD = (float) 0.06;
    /** The priority threshold for operation to be executed. */
    public float getPriorityThreshold() {
    	return PRIORITY_THRESHOLD;
    }

    protected float DEFAULT_JUDGMENT_CONFIDENCE = (float) 0.9;
    /** Default confidence of input judgment. */
    public float getDefaultJudgmentConfidence() {
		return DEFAULT_JUDGMENT_CONFIDENCE;
	}
    
    protected float DEFAULT_JUDGMENT_PRIORITY = (float) 0.8;
    /** Default priority of input judgment */
	public float getDefaultJudgmentPriority() {
		return DEFAULT_JUDGMENT_PRIORITY;
	}

    protected float DEFAULT_JUDGMENT_DURABILITY = (float) 0.8;
    /** Default durability of input judgment */
	public float getDefaultJudgmentDurability() {
		return DEFAULT_JUDGMENT_DURABILITY;
	}
	
    protected float DEFAULT_GOAL_PRIORITY = (float) 0.9;
    /** Default priority of input goal */
	public float getDefaultGoalPriority() {
		return DEFAULT_GOAL_PRIORITY;
	}
	
    protected float DEFAULT_GOAL_DURABILITY = (float) 0.7;
    /** Default durability of input goal */
	public float getDefaultGoalDurability() {
		return DEFAULT_GOAL_DURABILITY;
	}

 	protected float DEFAULT_QUESTION_PRIORITY = (float) 0.9;
    /** Default priority of input question */
	public float getDefaultQuestionPriority() {
		return DEFAULT_QUESTION_PRIORITY;
	}
	
    protected float DEFAULT_QUESTION_DURABILITY = (float) 0.7;
    /** Default durability of input question */
	public float getDefaultQuestionDurability() {
		return DEFAULT_QUESTION_DURABILITY;
	}
	
    protected int CONCEPT_FORGETTING_CYCLE = 3;
    /** Concept decay rate in ConceptBag, in [1, 99]. */
	public int getConceptForgetRate() {
		return CONCEPT_FORGETTING_CYCLE;
	}
	
    protected float LOAD_FACTOR = (float) 0.5;
    /** Hashtable load factor in Bag */
	public float getBagLoadFactor() {
		return LOAD_FACTOR;
	}

    protected int BAG_THRESHOLD = 10;
    /** Level separation in Bag, one digit, for display (run-time adjustable) and management (fixed) */
	public int getBagThreshold() {
		return BAG_THRESHOLD;
	}

    protected int BAG_LEVEL = 100;
    /** Level granularity in Bag, two digits */
	public int getBagLevel() {
		return BAG_LEVEL;
	}

    protected int CONCEPT_BAG_SIZE = 1000;
    /** Size of ConceptBag */
    public int getConceptBagSize() {
		return CONCEPT_BAG_SIZE;
	}

    protected int NEW_TASK_FORGETTING_CYCLE = 1;
    /** Task decay rate in TaskBuffer, in [1, 99]. */
	public int getNewTaskForgetRate() {
		return NEW_TASK_FORGETTING_CYCLE;
	}

    protected int TASK_BUFFER_SIZE = 20;
    /** Size of TaskBuffer */
	public int getTaskBufferSize() {
		return TASK_BUFFER_SIZE;
	}

    protected int TASK_LINK_FORGETTING_CYCLE = 10;
    /** TaskLink decay rate in TaskLinkBag, in [1, 99]. */
	public int getTaskLinkForgetRate() {
		return TASK_LINK_FORGETTING_CYCLE;
	}

    protected int TASK_LINK_BAG_SIZE = 20;
    /** Size of TaskLinkBag */
	public int getTaskLinkBagSize() {
		return TASK_LINK_BAG_SIZE;
	}

    protected int TERM_LINK_FORGETTING_CYCLE = 50;
    /** TermLink decay rate in TermLinkBag, in [1, 99]. */
	public int getTermLinkForgetRate() {
		return TERM_LINK_FORGETTING_CYCLE;
	}

    protected int TERM_LINK_BAG_SIZE = 100;
    /** Size of TermLinkBag */
	public int getTermLinkBagSize() {
		return TERM_LINK_BAG_SIZE;
	}
	
    protected float BUDGET_THRESHOLD = (float) 0.001;
    /** The budget threthold for task to be accepted. */
    public float getBudgetThreshold() {
		return BUDGET_THRESHOLD;
	}

    protected int MAXMUM_GOALS_LENGTH = 5;

    /** Maximum number of goals kept in a Concept */
    public int getMaximumGoalsLength() {
		return MAXMUM_GOALS_LENGTH;
	}

    protected int MAXMUM_BELIEF_LENGTH = 8;
    /** Maximum number of beliefs kept in a Concept */
	public int getMaximumBeliefLength() {
		return MAXMUM_BELIEF_LENGTH;
	}
	
    protected int MAX_REASONED_TERM_LINK = 3;

    /** Maximum TermLinks used in reasoning for each Task in Concept */
	public int getMaxReasonedTermLink() {
		return MAX_REASONED_TERM_LINK;
	}


}
