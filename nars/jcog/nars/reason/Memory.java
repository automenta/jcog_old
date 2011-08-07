package jcog.nars.reason;

import java.security.Policy.Parameters;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.logging.Logger;
import jcog.nars.BudgetValue;
import jcog.nars.Concept;
import jcog.nars.Judgment;
import jcog.nars.NARTruth;
import jcog.nars.Question;
import jcog.nars.Sentence;
import jcog.nars.Stamp;
import jcog.nars.Task;
import jcog.nars.TaskLink;
import jcog.nars.TermLink;
import jcog.nars.reason.inference.BudgetFunctions;
import jcog.nars.reason.inference.SyllogisticRules;
import jcog.nars.reason.io.Record;
import jcog.nars.reason.io.Symbols;
import jcog.nars.reason.language.CompoundTerm;
import jcog.nars.reason.language.Conjunction;
import jcog.nars.reason.language.Equivalence;
import jcog.nars.reason.language.Implication;
import jcog.nars.reason.language.Inheritance;
import jcog.nars.reason.language.Statement;
import jcog.nars.reason.language.Term;
import jcog.nars.reason.operation.Operator;
import jcog.nars.reason.store.ConceptBag;
import jcog.nars.reason.store.TaskBuffer;

/**
 * The memory of the system.
 */
public class Memory {

    public final Logger logger;
    
    /* ---------- all members are static, with limited space ---------- */
    /** Concept bag. Containing all Concepts of the system */
    private ConceptBag concepts;
    /** Operators (built-in terms) table. Accessed by name */
    private  HashMap<String, Operator> operators;
    /* There is no global Term table, which may ask for unlimited space. */
    /** List of inference newTasks, to be processed in the next working cycle */
    private ArrayList<Task> newTasks;
    /** New tasks with novel composed terms, for delayed and selective processing */
    private TaskBuffer novelTasks;
    /** List of recent events, for temporal learning */
    private TaskBuffer recentEvents;
    /* ---------- global variables used to record emotional values ---------- */
    /** average desire-value */
    private float happy;
    /** average priority */
    private float busy;
    
    //TODO make these NOT 'public'
    
    /* ---------- global variables used to reduce method arguments ---------- */
    /** Shortcut to the selected Term */
    public Term currentTerm;
    /** Shortcut to the selected TaskLink */
    public TaskLink currentTaskLink;
    /** Shortcut to the selected Task */
    public Task currentTask;
    /** Shortcut to the selected TermLink */
    public TermLink currentBeliefLink;
    /** Shortcut to the selected belief */
    public Judgment currentBelief;
    /** Shortcut to the derived Stamp */
    public Stamp newStamp;
    
    /* ---------- global variables used to record emotional values ---------- */

    
    private final NARParams params;
    private Stamp currentStamp;

    public Memory(NARParams params) {
    	super();
    	this.params = params;
        logger = Logger.getLogger(this.toString());
    }

    public Logger getLogger() {
        return logger;
    }        
    
    /* ---------- initialization ---------- */
    /**
     * Initialize a new memory by creating all members.
     * <p>
     * Called in Center.reset only
     */
    public void init() {
        concepts = new ConceptBag(params);            // initially empty, with capacity limit
        operators = Operator.setOperators();    // with operators created, then remain constant
        newTasks = new ArrayList<Task>();       // initially empty, without capacity limit
        novelTasks = new TaskBuffer(params);        // initially empty, with capacity limit
        recentEvents = new TaskBuffer(params);      // with a different capacity?
        happy = 0.5f;
        busy = 0.5f;
    }

    public NARParams getParams() {
        return params;
    }

    public Task getCurrentTask() {
        return currentTask;
    }

    
    /* ---------- access utilities ---------- */
    /**
     * Get a Term for a given name of a Concept or Operator
     * <p>
     * called in StringParser and the make methods of compound terms.
     * @param name the name of a concept or operator
     * @return a Term or null (if no Concept/Operator has this name)
     */
    public Term nameToListedTerm(String name) {
        Concept concept = concepts.get(name);
        if (concept != null) {
            return concept.getTerm();
        }
        return operators.get(name);
    }

    /**
     * Check if a string is an operator name
     * <p>
     * called in StringParser only.
     * @param name the name of a possible operator
     * @return the corresponding operator or null
     */
    public Operator nameToOperator(String name) {
        return operators.get(name);
    }

    /**
     * Get an existing Concept for a given name
     * <p>
     * called from Term and ConceptWindow.
     * @param name the name of a concept
     * @return a Concept or null
     */
    public Concept nameToConcept(String name) {
        return concepts.get(name);
    }

    /**
     * Get an existing Concept for a given Term.
     * @param term The Term naming a concept
     * @return a Concept or null
     */
    public Concept termToConcept(Term term) {
        return nameToConcept(term.getName());
    }

    /**
     * Get the Concept associated to a Term, or create it.
     * @param term indicating the concept
     * @return an existing Concept, or a new one
     */
    public Concept getConcept(Term term) {
        String n = term.getName();
        Concept concept = concepts.get(n);
        if (concept == null) {
            concept = new Concept(params, term); // the only place to make a new Concept
            boolean created = concepts.putIn(concept);
            if (!created) {
                return null;
            }
        }
        return concept;
    }

    /**
     * Adjust the activation level of a Concept
     * <p>
     * called in Concept.insertTaskLink only
     * @param c the concept to be adusted
     * @param b the new BudgetValue
     */
    public void activateConcept(Concept c, BudgetValue b) {
        concepts.pickOut(c.getKey());
        BudgetFunctions.activate(c, b);
        concepts.putBack(c);
    }

    /**
     * Check if there is already any derived Task in the current cycle
     * <p>
     * called in Concept.fire only
     * @return Whether in the current cycle there is already derived Task
     */
    public boolean noResult() {
        return (newTasks.size() == 0);
    }

//    public Term mostActive() {        
//        return concepts.mostActive();
//    }

    /* ---------- new task entries ---------- */
    /* There are several types of new tasks, all added into the 
    newTasks list, to be processed in the next cycle.
    Some of them are reported and/or logged. */
    /**
     * Input task processing. Invoked by the outside or inside enviornment.
     * Outside: StringParser (input); Inside: Operator (feedback).
     * Input tasks with low priority are ignored, and the others are put into task buffer.
     * @param task The input task
     */
    public void inputTask(Task task) {
        if (task.aboveThreshold(this)) {
            getLogger().info("!!! Perceived: " + task + "\n");
            report(task.getSentence(), true);    // report input
            newTasks.add(task);       // wait to be processed in the next cycle
        } else {
            getLogger().info("!!! Neglected: " + task + "\n");
        }
    }

    /**
     * Reporting executed task, and rememberAction the event
     * <p>
     * called from Operator.call only
     * @param task the executed task
     */
    public void executedTask(Task task) {
        getLogger().info("!!! Executed: " + task.getSentence() + "\n");
//        Goal g = (Goal) task.getSentence();
//        Judgment j = new Judgment(g.cloneContent());   // perceived execution
//        Task newTask = new Task(j, task.getBudget());
//        report(newTask.getSentence(), false);
//        newTasks.add(newTask);
    }

    /**
     * To rememberAction an internal action as an operation
     * <p>
     * called from Concept
     * @param task The task processed
     */
    public void rememberAction(Task task) {
        Term content = task.getContent();
        if (content instanceof Inheritance) {
            if (((Inheritance) content).parseOperation(null) != null)
                return;     // to present infinite recursions
        }
        Sentence sentence = task.getSentence();
        NARTruth truth = new NARTruth(1.0f, params.getDefaultJudgmentConfidence());
        Stamp stamp = (Stamp) task.getSentence().getStamp().clone();
        stamp.setEventTime(Center.getTime());
        Judgment j = new Judgment(sentence.toTerm(), truth, stamp);
        Task newTask = new Task(j, task.getBudget());
        getLogger().info("Remembered: " + j.toString());
        newTasks.add(newTask);
    }

    /**
     * Activated task called in MatchingRules.trySolution and Concept.processGoal
     * @param budget The budget value of the new Task
     * @param sentence The content of the new Task
     * @param isInput Whether the question is input
     */
    public void activatedTask(BudgetValue budget, Sentence sentence, boolean isInput) {
        Task task = new Task(sentence, budget);
        getLogger().info("!!! Activated: " + task.toString() + "\n");
        if (sentence instanceof Question) {
            float s = task.getBudget().summary();
            @SuppressWarnings("static-access")
            float minSilent = Center.mainWindow.silentW.value() / 100.0f;
            if (s > minSilent) {  // only report significient derived Tasks
                report(task.getSentence(), false);
            }
        }
        newTasks.add(task);
    }

    /**
     * Derived task comes from the inference rules.
     * @param task the derived task
     */
    private void derivedTask(Task task) {
        if (task.aboveThreshold(this)) {
            getLogger().info("!!! Derived: " + task + "\n");
            float budget = task.getBudget().summary();
            @SuppressWarnings("static-access")
            float minSilent = Center.mainWindow.silentW.value() / 100.0f;
            if (budget > minSilent) {  // only report significient derived Tasks
                report(task.getSentence(), false);
            }
            newTasks.add(task);
            rememberInference(task);
        } else {
            getLogger().info("!!! Ignored: " + task + "\n");
        }
    }

    private void rememberInference(Task derived) {
        Sentence sentence = derived.getSentence();
        NARTruth t1 = sentence.getTruth();
        if ((t1 != null) && t1.toWord().equals("UNSURE")) {
            return;
        }
        Term pred = sentence.getContent();
        if (!((pred instanceof Inheritance) && ((Inheritance) pred).parseOperation(null) != null)) {
             pred = sentence.toTerm();
        }
        Term subj = currentTask.getSentence().getContent();
        if (!((subj instanceof Inheritance) && ((Inheritance) subj).parseOperation(null) != null)) {
             subj = currentTask.getSentence().toTerm();
        }
        if (currentBelief != null) {
            Term b = currentBelief.toTerm();
            Term c = Conjunction.make(this, subj, b, -1);
            subj = c;
        } else {
            return;     // temporary solution
        }
        Term imp = Implication.make(subj, pred, false, 0);
        if (imp != null) {
            NARTruth truth = new NARTruth(1.0f, params.getDefaultJudgmentConfidence());
            Judgment j = new Judgment(imp, truth, sentence.getStamp());
            Task newTask = new Task(j, derived.getBudget());
            getLogger().info("Remembered: " + j.toString());
            newTasks.add(newTask);
        }
    }

    /* --------------- new task building --------------- */
    /**
     * Shared final operations by all double-premise rules, called from the rules except StructuralRules
     * @param budget The budget value of the new task
     * @param content The content of the new task
     * @param truth The truth value of the new task
     * @param premise1 The first premise to record in the new Judgment
     * @param premise2 The second premise to record in the new Judgment
     */
    public void doublePremiseTask(BudgetValue budget, Term content, NARTruth truth,
            Sentence premise1, Sentence premise2) {
        if (content != null) {
            Sentence newSentence = Sentence.make(currentTask.getSentence(), content, truth, (Stamp) newStamp.clone(), premise1, premise2);
            Task newTask = new Task(newSentence, budget);
            derivedTask(newTask);
        }
    }

    public void predictionWithConfirmation(BudgetValue budget, Term content, NARTruth truth,
            Sentence premise1, Sentence premise2) {
        if (content != null) {
            Sentence newSentence = Sentence.make(currentTask.getSentence(), content, truth, newStamp, premise1, premise2);
            Task newTask = new Task(newSentence, budget);
            derivedTask(newTask);
            Question ques = new Question((Judgment) newSentence);
            newTask = new Task(ques, budget);
            derivedTask(newTask);
        }
    }

    public void lackConfirmation(Term content, long t) {
        NARTruth tv = new NARTruth(0f, 2*Parameters.DEFAULT_CONFIRMATION_EXPECTATION - 1);
        BudgetValue bv = new BudgetValue(Parameters.DEFAULT_JUDGMENT_PRIORITY, Parameters.DEFAULT_JUDGMENT_DURABILITY, 1);
        Stamp st = new Stamp();
        st.setEventTime(t);
        Judgment j = new Judgment(content, '.', tv, st, null, null);
        Task newTask = new Task(j, bv);
        derivedTask(newTask);
    }

    /**
     * The final operations of the revision rules, called from MatchingRules
     * @param budget The budget value of the new task
     * @param content The content of the new task
     * @param truth The truth value of the new task
     * @param premise1 The first premise to record in the new Judgment. May be null.
     * @param premise2 The second premise to record in the new Judgment. May be null.
     */
    public void revisionTask(BudgetValue budget, Term content, NARTruth truth,
            Sentence premise1, Sentence premise2) {
        if (content != null) {
            Sentence newSentence = Sentence.make(currentTask.getSentence(), content, truth, newStamp, premise1, premise2);
            Task newTask = new Task(newSentence, budget);
            if (currentTask.isStructural()) {
                newTask.setStructural();
            }
            derivedTask(newTask);
        }
    }

    /**
     * Shared final operations by all single-premise rules, called in StructuralRules
     * @param budget The budget value of the new task
     * @param content The content of the new task
     * @param truth The truth value of the new task
     * @param premise The premise to record in the new Judgment
     */
    public void singlePremiseTask(BudgetValue budget, Term content, NARTruth truth, Sentence premise) {
        Sentence sentence = currentTask.getSentence();
        Sentence newSentence = Sentence.make(sentence, content, truth, (Stamp) sentence.getStamp().clone(), premise, null);
        Task newTask = new Task(newSentence, budget);
        newTask.setStructural();
        derivedTask(newTask);
    }

    /**
     * Convert jusgment into different relation
     * <p>
     * called in MatchingRules
     * @param budget The budget value of the new task
     * @param truth The truth value of the new task
     */
    public void convertedJudgment(NARTruth truth, BudgetValue budget) {
        Statement content = (Statement) Memory.currentTask.getContent();
        Statement beliefContent = (Statement) Memory.currentBelief.getContent();
        Term subj = content.getSubject();
        Term pred = content.getPredicate();
        Term subjB = beliefContent.getSubject();
        Term predB = beliefContent.getPredicate();
        Term otherTerm;
        if (subj.getName().indexOf(Symbols.QUERY_VARIABLE_TAG) >= 0) {
//        if ((subj instanceof Variable) && (((Variable) subj).getType() == Variable.VarType.QUERY)) {
            otherTerm = (pred.equals(subjB)) ? predB : subjB;
            content = Statement.make(content, otherTerm, pred);
        }
        if (pred.getName().indexOf(Symbols.QUERY_VARIABLE_TAG) >= 0) {
//        if ((pred instanceof Variable) && (((Variable) pred).getType() == Variable.VarType.QUERY)) {
            otherTerm = (subj.equals(subjB)) ? predB : subjB;
            content = Statement.make(content, subj, otherTerm);
        }
        Stamp stamp = (Stamp) Memory.currentBelief.getStamp().clone();
        Sentence newJudgment = Sentence.make(content, Symbols.JUDGMENT_MARK, truth, stamp, Memory.currentBelief, null);
        Task newTask = new Task(newJudgment, budget);
        newTask.setStructural();
        derivedTask(newTask);
    }

    /* ---------- system working cycle ---------- */
    /**
     * An atomic working cycle of the system: process new Tasks, then fire a concept
     * <p>
     * Called from Center.tick only
     * <p>
     * In the future, it is possible to adjust the occuring ratio of the two actions
     */
    public void cycle() {
        processNewTask();
        if (noResult()) {
            processNovelTask();
        }
        if (noResult()) {
            processConcept();
        }
        novelTasks.refresh();
    }

    /**
     * Process the newTasks accumulated in the previous cycle, accept input ones
     * and those that corresponding to existing concepts, plus one from the buffer.
     */
    private void processNewTask() {
        Task task;
        int counter = newTasks.size();  // don't include new tasks produced in the current cycle
        while (counter-- > 0) {
            task = newTasks.remove(0);
            adjustBusy(task.summary(), task.getPriority());
            if (task.getSentence().isInput() || (termToConcept(task.getContent()) != null)) { // new input or existing concept
                immediateProcess(task);
            } else {
                novelTasks.putIn(task);    // delayed processing
            }
        }
    }

    private void processNovelTask() {
        Task task = novelTasks.takeOut();       // select a task from novelTasks
        if (task != null) {
            immediateProcess(task);
        }
    }

    /**
     * Select a concept to fire.
     */
    private void processConcept() {
        resetCurrent();
        Concept currentConcept = concepts.takeOut();
        if (currentConcept != null) {
            currentTerm = currentConcept.getTerm();
            getLogger().info(" * Selected Concept: " + currentTerm + "\n");
            concepts.putBack(currentConcept);   // current Concept remains in the bag all the time
            currentConcept.fire(this);              // a working cycle
        }
    }

    private void resetCurrent() {
        currentTerm = null;
        currentTaskLink = null;
        currentTask = null;
        currentBeliefLink = null;
        currentBelief = null;
        newStamp = null;
    }

    /* ---------- task processing ---------- */
    /**
     * Immediate processing of a new task, in constant time
     * Local processing, in one concept only
     * @param task the task to be accepted
     */
    private void immediateProcess(Task task) {
        resetCurrent();
        currentTask = task; // one of the two places where this variable is set
        getLogger().info("!!! Insert: " + task + "\n");
        Term content = task.getContent();
        Concept c = getConcept(content);
        if (c != null) {
            c.directProcess(task, new Date().getTime(), this);
        }
    }

    /**
     * Link to a new task from all relevant concepts for continued processing in
     * the near future for unspecified time.
     * <p>
     * The only method that calls the TaskLink constructor.
     * @param task The task to be linked
     * @param content The content of the task
     */
    public void linkToTask(Task task, Term content, Concept contentConcept) {
        BudgetValue budget = task.getBudget();
        TaskLink taskLink = new TaskLink(task, null, budget);   // link type: SELF
        contentConcept.insertTaskLink(taskLink);
        if (content instanceof CompoundTerm) {
            ArrayList<TermLink> termLinks = (contentConcept != null)
                    ? contentConcept.getTermLinkTemplates()
                    : ((CompoundTerm) content).prepareComponentLinks();  // use saved if exist
            if (termLinks.size() > 0) {
                BudgetValue subBudget = BudgetFunctions.distributeAmongLinks(budget, termLinks.size());
                if (subBudget.aboveThreshold(this)) {
                    Term componentTerm;
                    Concept componentConcept;
                    for (TermLink termLink : termLinks) {
                        if (!(task.isStructural() && (termLink.getType() == TermLink.TRANSFORM))) { // avoid circular transform
                            taskLink = new TaskLink(task, termLink, subBudget);
                            componentTerm = termLink.getTarget();
                            componentConcept = getConcept(componentTerm);
                            if (componentConcept != null) {
                                componentConcept.insertTaskLink(taskLink);
                            }
                        }
                    }
                    contentConcept.buildTermLinks(budget);  // recursively insert TermLink
                }
            }
        }
    }

    /**
     * Simple temporal regularity discovery [To be refined]
     * <p>
     * called in Memory.immediateProcess
     * @param event1 A new event
     */
    public void eventProcessing(Task event1) {
        Task event2 = recentEvents.takeOut();
        if (event2 != null) {
            if (event1.getSentence().noOverlapping(this, event2.getSentence())) {
                SyllogisticRules.temporalIndCom(event1, event2);
            }
            recentEvents.putBack(event2);
        }
        Term content = event1.getContent();
        if (!(content instanceof Implication) && !(content instanceof Equivalence)) {
            recentEvents.putIn(event1);
        }
    }

    /* ---------- status evaluation ---------- */

    public float happyValue() {
        return happy;
    }

    public float busyValue() {
        return busy;
    }

    public void adjustHappy(float newValue, float weight) {
        float oldV = happy;
        happy += newValue * weight;
        happy /= 1.0f + weight;
        if (Math.abs(oldV - happy) > 0.1) {
            getLogger().info("HAPPY: " + (int) (oldV*10.0) + " to " + (int) (happy*10.0) + "\n");
        }
    }
    public void adjustBusy(float newValue, float weight) {
        float oldV = busy;
        busy += newValue * weight;
        busy /= (1.0f + weight);
        if (Math.abs(oldV - busy) > 0.1) {
            getLogger().info("BUSY: " + (int) (oldV*10.0) + " to " + (int) (busy*10.0) + "\n");
        }
    }



    /**
     * Prepare buffered tasks for display, called from TaskBuffer.
     * @return the tasks as a String
     */
    public String newTasksToString() {
        String s = " New Tasks: \n";
        for (int i = 0; i < newTasks.size(); i++) {
            s += newTasks.get(i) + "\n";
        }
        s += "\n Task Buffer: \n";
        return s;
    }

    /**
     * Display input/output sentence in the MainWindow.
     * @param sentence the sentence to be displayed
     * @param input whether the task is input
     */
    public static void report(Sentence sentence, boolean input) {
        String s;
        if (input) {
            s = "  IN: ";
        } else {
            s = " OUT: ";
        }
        s += sentence.toString() + "\n";
        System.out.println(s);
        //Center.mainWindow.post(s);
    }

    public void setCurrentStamp(Stamp s) {
        currentStamp = s;
    }

    public Stamp getCurrentStamp() {
        return currentStamp;
    }
    
    
    
}

//public class Memory {
//
//    /** Concept bag. Containing all Concepts of the system */
//    private ConceptBag concepts;
//    
//    /** Operators (built-in terms) table. Accessed by name */
//    private static HashMap<String, Operator> operators;
//
//    /* There is no global Term table, which may ask for unlimited space. */
//    /** List of inference newTasks, to be processed in the next working cycle */
//    private ArrayList<Task> newTasks;
//    
//    /** New tasks with novel composed terms, for delayed and selective processing */
//    private TaskBuffer novelTasks;
//    
//    /** List of recent events, for temporal learning */
//    private TaskBuffer recentEvents;
//    
//    /** Shortcut to the selected Term */
//    public Term currentTerm;
//    /** Shortcut to the selected TaskLink */
//    public TaskLink currentTaskLink;
//    /** Shortcut to the selected Task */
//    public Task currentTask;
//    /** Shortcut to the selected TermLink */
//    public TermLink currentBeliefLink;
//    /** Shortcut to the selected belief */
//    public Judgment currentBelief;
//    /** Shortcut to the derived Stamp */
//    public Stamp currentStamp;
//    /** Shortcut to the derived tense */
//    public TemporalValue currentTense;
//    
//	private NARParams params;
//
//    public Memory(NARParams params) {
//    	super();
//    	this.params = params;
//	}
//
//    public NARParams getParams() {
//		return params;
//	}
//    
//	/* ---------- initialization ---------- */
//    /**
//     * Initialize a new memory by creating all members.
//     * <p>
//     * Called in Center.reset only
//     */
//    public void init() {
//        operators = Operator.setOperators();    // with operators created, then remain constant
//        newTasks = new ArrayList<Task>();       // initially empty, without capacity limit
//
//    	concepts = new ConceptBag(getParams());            // initially empty, with capacity limit
//        novelTasks = new TaskBuffer(getParams());        // initially empty, with capacity limit
//        recentEvents = new TaskBuffer(getParams());      // with a different capacity?
//    }
//
//    /* ---------- access utilities ---------- */
//    /**
//     * Get a Term for a given name of a Concept or Operator
//     * <p>
//     * called in StringParser and the make methods of compound terms.
//     * @param name the name of a concept or operator
//     * @return a Term or null (if no Concept/Operator has this name)
//     */
//    public Term nameToListedTerm(String name) {
//        Concept concept = concepts.get(name);
//        if (concept != null) {
//            return concept.getTerm();
//        }
//        return operators.get(name);
//    }
//
//    /**
//     * Check if a string is an operator name
//     * <p>
//     * called in StringParser only.
//     * @param name the name of a possible operator
//     * @return the corresponding operator or null
//     */
//    public Operator nameToOperator(String name) {
//        return operators.get(name);
//    }
//
//    /**
//     * Get an existing Concept for a given name
//     * <p>
//     * called from Term and ConceptWindow.
//     * @param name the name of a concept
//     * @return a Concept or null
//     */
//    public Concept nameToConcept(String name) {
//        return concepts.get(name);
//    }
//
//    /**
//     * Get an existing Concept for a given Term.
//     * @param term The Term naming a concept
//     * @return a Concept or null
//     */
//    public Concept termToConcept(Term term) {
//        return nameToConcept(term.getName());
//    }
//
//    /**
//     * Get the Concept associated to a Term, or create it.
//     * @param term indicating the concept
//     * @return an existing Concept, or a new one
//     */
//    public Concept getConcept(Term term) {
//        String n = term.getName();
//        Concept concept = concepts.get(n);
//        if (concept == null) {
//            concept = new Concept(getParams(), term); // the only place to make a new Concept
//            concepts.putIn(concept);
//        }
//        return concept;
//    }
//
//    /**
//     * Adjust the activation level of a Concept
//     * <p>
//     * called in Concept.insertTaskLink only
//     * @param c the concept to be adusted
//     * @param b the new BudgetValue
//     */
//    public void activateConcept(Concept c, BudgetValue b) {
//        concepts.pickOut(c.getKey());
//        BudgetFunctions.activate(c, b);
//        concepts.putBack(c);
//    }
//
//    /**
//     * Check if there is already any derived Task in the current cycle
//     * <p>
//     * called in Concept.fire only
//     * @return Whether in the current cycle there is already derived Task
//     */
//    public boolean noResult() {
//        return (newTasks.size() == 0);
//    }
//
//    /* ---------- new task entries ---------- */
//    /* There are several types of new tasks, all added into the 
//    newTasks list, to be processed in the next cycle.
//    Some of them are reported and/or logged. */
//    /**
//     * Input task processing.
//     * @param task the input task
//     */
//    public void inputTask(Task task) {
//        //Record.append("!!! Input: " + task + "\n");
//        
//        if (task.aboveThreshold(getParams().getBudgetThreshold())) {
//            report(task.getSentence(), true);    // report input
//            newTasks.add(task);       // wait to be processed in the next cycle
//            novelTasks.refresh();           // refresh display
//        } else {
//        	
//            //Record.append("!!! Ignored: " + task + "\n");
//            
//        }
//    }
//
//    /**
//     * Derived task comes from the inference rules.
//     * @param task the derived task
//     */
//    private void derivedTask(Task task) {
//        //Record.append("!!! Derived: " + task + "\n");
//    	
//        if (task.aboveThreshold()) {
//            float budget = task.getBudget().summary();
//            
////            float minSilent = Center.mainWindow.silentW.value() / 100.0f;
////            
////            if (budget > minSilent) {  // only report significient derived Tasks
////                report(task.getSentence(), false);
////            }
//            
//            newTasks.add(task);
//            novelTasks.refresh();
//        } else {
//            //Record.append("!!! Ignored: " + task + "\n");
//        }
//    }
//
//    /**
//     * Reporting executed task, and remember the event
//     * <p>
//     * called from Operator.call only
//     * @param task the executed task
//     */
//    public void executedTask(Task task) {
//        //Record.append("!!! Executed: " + task.getSentence() + "\n");
//    	
//        Goal g = (Goal) task.getSentence();
//        Judgment j = new Judgment(g);
//        Task newTask = new Task(j, task.getBudget());
//        report(newTask.getSentence(), false);
//        newTasks.add(newTask);
//        novelTasks.refresh();
//    }
//
//    /**
//     * Activated task coming from MatchingRules.trySolution
//     * @param budget The budget value of the new Task
//     * @param sentence The content of the new Task
//     * @param isInput Whether the question is input
//     */
//    public void activatedTask(BudgetValue budget, Sentence sentence, boolean isInput) {
//        Task task = new Task(sentence, budget);
//        //Record.append("!!! Activated: " + task.toString() + "\n");
//        if (sentence instanceof Question) {
//            float s = task.getBudget().summary();
////            @SuppressWarnings("static-access")
////            float minSilent = Center.mainWindow.silentW.value() / 100.0f;
////            if (s > minSilent) {  // only report significient derived Tasks
////                report(task.getSentence(), false);
////            }
//        }
//        newTasks.add(task);
//        novelTasks.refresh();
//    }
//
//    /* --------------- new task building --------------- */
//    /**
//     * Shared final operations by all double-premise rules, called from the rules except StructuralRules
//     * @param budget The budget value of the new task
//     * @param content The content of the new task
//     * @param truth The truth value of the new task
//     */
//    public void doublePremiseTask(BudgetValue budget, Term content, NARTruth truth) {
//        if (content != null) {
//            Sentence newSentence = Sentence.make(currentTask.getSentence(), content, truth, currentStamp, currentTense);
//            Task newTask = new Task(newSentence, budget);
//            derivedTask(newTask);
//        }
//    }
//
//    /**
//     * Shared final operations by all single-premise rules, called in StructuralRules
//     * @param budget The budget value of the new task
//     * @param content The content of the new task
//     * @param truth The truth value of the new task
//     */
//    public void singlePremiseTask(BudgetValue budget, Term content, NARTruth truth) {
//        Sentence sentence = currentTask.getSentence();
//        Sentence newSentence = Sentence.make(sentence, content, truth, sentence.getStamp(), sentence.getTense());
//        Task newTask = new Task(newSentence, budget);
//        newTask.setStructual();
//        derivedTask(newTask);
//    }
//
//    /**
//     * Convert jusgment into different relation
//     * <p>
//     * called in MatchingRules
//     * @param budget The budget value of the new task
//     * @param truth The truth value of the new task
//     */
//    public void convertedJudgment(NARTruth truth, BudgetValue budget) {
//        Term content = currentTask.getContent();
//        TemporalValue tense = currentBelief.getTense();
//        Stamp stamp = currentBelief.getStamp();
//        Sentence newJudgment = Sentence.make(content, Symbols.JUDGMENT_MARK, truth, stamp, tense);
//        Task newTask = new Task(newJudgment, budget);
//        newTask.setStructual();
//        derivedTask(newTask);
//    }
//
//    /* ---------- system working cycle ---------- */
//    /**
//     * An atomic working cycle of the system: process new Tasks, then fire a concept
//     * <p>
//     * Called from Center.tick only
//     * @param now 
//     * @param params 
//     */
//	public void cycle(long now) {
//        // keep the following order
//        processTask(now);
//        processConcept();
//    }
//
//    /**
//     * Process the newTasks accumulated in the previous cycle, accept input ones
//     * and those that corresponding to existing concepts, plus one from the buffer.
//     * @param now 
//     * @param params 
//     */
//    private void processTask(long now) {
//        Task task;
//        int counter = newTasks.size();  // don't include new tasks produced in the current cycle
//        while (counter-- > 0) {
//            task = newTasks.remove(0);
//            if (task.getSentence().isInput() || (termToConcept(task.getContent()) != null)) { // new input or existing concept
//                immediateProcess(task, now);
//                if (task.getSentence().isInput() && (task.getSentence() instanceof Question)) {
//                	
////                    Concept concept = nameToConcept(task.getSentence().getContent().getID());
////                    if (concept != null) {
////                        concept.startPlay(false);
////                    }
//                    
//                }
//            } else {
//                novelTasks.putIn(task);    // delayed processing
//            }
//        }
//        task = novelTasks.takeOut();       // select a task from novelTasks
//        if (task != null) {
//            immediateProcess(task, now);
//        }
//    }
//
//    /**
//     * Select a concept to fire.
//     */
//    private void processConcept() {
//        Concept currentConcept = concepts.takeOut();
//        if (currentConcept != null) {
//            currentTerm = currentConcept.getTerm();
//            
//            //Record.append(" * Selected Concept: " + currentTerm + "\n");
//            
//            concepts.putBack(currentConcept);   // current Concept remains in the bag all the time
//            currentConcept.fire(this);              // a working cycle
//            novelTasks.refresh();          // show new result
//        }
//    }
//
//    /* ---------- task processing ---------- */
//    /**
//     * Imediate processing of a new task, in constant time
//     * Local processing, in one concept only
//     * @param task the task to be accepted
//     * @param clock 
//     * @param params 
//     */
//    private void immediateProcess(Task task, long now) {
//        currentTask = task;
//        
//        //Record.append("!!! Insert: " + task + "\n");
//        
//        Term content = task.getContent();
//        Concept c = getConcept(content);
//        c.directProcess(task, now, getParams());
//        if (task.aboveThreshold(getParams().getBudgetThreshold())) {    // still need to be processed
//            continuedProcess(task, content);
//            Sentence s = task.getSentence();
//            if (s.isJudgment() && (s.getTense() != null)) {
//                eventProcessing(task);
//            }
//        }
//    }
//    
//
//    /**
//     * Link to a new task from all relevant concepts for continued processing in
//     * the near future for unspecified time.
//     * <p>
//     * The only method that calls the TaskLink constructor.
//     * @param task The task to be linked
//     * @param content The content of the task
//     */
//    private void continuedProcess(Task task, Term content) {
//        TaskLink taskLink;
//        Concept contentConcept = getConcept(content);
//        BudgetValue budget = task.getBudget();
//        taskLink = new TaskLink(task, null, budget, getParams().getTermLinkRecordLength());   // link type: SELF
//        
//        contentConcept.insertTaskLink(taskLink);
//        activateConcept(contentConcept, budget);       // activate the concept
//
//        
//        if (content instanceof CompoundTerm) {
//            ArrayList<TermLink> termLinks = (contentConcept != null) ? contentConcept.getTermLinkTemplates()
//                    : ((CompoundTerm) content).prepareComponentLinks();  // use saved if exist
//            if (termLinks.size() > 0) {
//                BudgetValue subBudget = BudgetFunctions.distributeAmongLinks(budget, termLinks.size());
//                if (subBudget.aboveThreshold(getParams().getBudgetThreshold())) {
//                    Term componentTerm;
//                    Concept componentConcept;
//                    for (TermLink termLink : termLinks) {
//                        if (!(task.isStructual() && (termLink.getType() == TermLink.TRANSFORM))) { // avoid circular transform
//                            taskLink = new TaskLink(task, termLink, subBudget, getParams().getTermLinkRecordLength());
//                            componentTerm = termLink.getTarget();
//                            componentConcept = getConcept(componentTerm);
//                            componentConcept.insertTaskLink(taskLink);
//                        }
//                    }
//                    contentConcept.buildTermLinks(this, budget);  // recursively insert TermLink
//                }
//            }
//        }
//    }
//
//    /**
//     * Simple temporal regularity discovery [To be refined]
//     * <p>
//     * called in Memory.immediateProcess
//     * @param event1 A new event
//     */
//    private void eventProcessing(Task event1) {
//        Task event2 = recentEvents.takeOut();
//        if ((event2 != null) && event1.getSentence().noOverlapping(event2.getSentence())) {
//            long time1 = event1.getSentence().getEventTime();
//            long time2 = event2.getSentence().getEventTime();
//            TemporalValue order = new TemporalValue((int) (time2 - time1));
//            SyllogisticRules.temporalIndCom(event1, event2, order);
//            recentEvents.putBack(event2);
//        }
//        recentEvents.putIn(event1);
//    }
//
////    /* ---------- display ---------- */
////    /**
////     * Display active concepts, called from MainWindow.
////     * @param s the window title
////     */
////    public void conceptsStartPlay(String s) {
////        concepts.startPlay(s);
////    }
//
////    /**
////     * Display newd tasks, called from MainWindow.
////     * @param s the window title
////     */
////    public void newTasksStartPlay(String s) {
////        novelTasks.startPlay(s);
////        recentEvents.startPlay("Recent Events");
////    }
//
//    /**
//     * Prepare buffered tasks for display, called from TaskBuffer.
//     * @return the tasks as a String
//     */
//    public String newTasksToString() {
//        String s = " New Tasks: \n";
//        for (int i = 0; i < newTasks.size(); i++) {
//            s += newTasks.get(i) + "\n";
//        }
//        s += "\n Task Buffer: \n";
//        return s;
//    }
//
//    /**
//     * Display input/output sentence in the MainWindow.
//     * @param sentence the sentence to be displayed
//     * @param input whether the task is input
//     */
//    public void report(Sentence sentence, boolean input) {
//        String s;
//        if (input) {
//            s = "  IN: ";
//        } else {
//            s = " OUT: ";
//        }
//        //s += sentence.toString2(this) + "\n";
//        s += sentence.toString() + "\n";
//        
//        //Center.mainWindow.post(s);
//    }
//
//    public ConceptBag getConcepts() {
//		return concepts;
//	}
//    public ArrayList<Task> getNewTasks() {
//		return newTasks;
//	}
//    public TaskBuffer getRecentEvents() {
//		return recentEvents;
//	}
//
//	public void notice(Object message) {
//		System.out.println(message);
//	}
//    
//}
