package jcog.nars.reason;

import java.util.ArrayList;
import java.util.HashMap;

import org.opencog.atom.nars.BudgetValue;
import org.opencog.atom.nars.Concept;
import org.opencog.atom.nars.Goal;
import org.opencog.atom.nars.Judgment;
import org.opencog.atom.nars.NARTruth;
import org.opencog.atom.nars.Question;
import org.opencog.atom.nars.Sentence;
import org.opencog.atom.nars.Stamp;
import org.opencog.atom.nars.Task;
import org.opencog.atom.nars.TaskLink;
import org.opencog.atom.nars.TemporalValue;
import org.opencog.atom.nars.TermLink;
import org.opencog.reason.nars.inference.BudgetFunctions;
import org.opencog.reason.nars.inference.SyllogisticRules;
import org.opencog.reason.nars.io.Symbols;
import org.opencog.reason.nars.language.CompoundTerm;
import org.opencog.reason.nars.language.Term;
import org.opencog.reason.nars.operation.Operator;
import org.opencog.reason.nars.store.ConceptBag;
import org.opencog.reason.nars.store.TaskBuffer;

public class Memory {

    /** Concept bag. Containing all Concepts of the system */
    private ConceptBag concepts;
    
    /** Operators (built-in terms) table. Accessed by name */
    private static HashMap<String, Operator> operators;

    /* There is no global Term table, which may ask for unlimited space. */
    /** List of inference newTasks, to be processed in the next working cycle */
    private ArrayList<Task> newTasks;
    
    /** New tasks with novel composed terms, for delayed and selective processing */
    private TaskBuffer novelTasks;
    
    /** List of recent events, for temporal learning */
    private TaskBuffer recentEvents;
    
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
    public Stamp currentStamp;
    /** Shortcut to the derived tense */
    public TemporalValue currentTense;
    
	private NARParams params;

    public Memory(NARParams params) {
    	super();
    	this.params = params;
	}

    public NARParams getParams() {
		return params;
	}
    
	/* ---------- initialization ---------- */
    /**
     * Initialize a new memory by creating all members.
     * <p>
     * Called in Center.reset only
     */
    public void init() {
        operators = Operator.setOperators();    // with operators created, then remain constant
        newTasks = new ArrayList<Task>();       // initially empty, without capacity limit

    	concepts = new ConceptBag(getParams());            // initially empty, with capacity limit
        novelTasks = new TaskBuffer(getParams());        // initially empty, with capacity limit
        recentEvents = new TaskBuffer(getParams());      // with a different capacity?
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
            concept = new Concept(getParams(), term); // the only place to make a new Concept
            concepts.putIn(concept);
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

    /* ---------- new task entries ---------- */
    /* There are several types of new tasks, all added into the 
    newTasks list, to be processed in the next cycle.
    Some of them are reported and/or logged. */
    /**
     * Input task processing.
     * @param task the input task
     */
    public void inputTask(Task task) {
        //Record.append("!!! Input: " + task + "\n");
        
        if (task.aboveThreshold(getParams().getBudgetThreshold())) {
            report(task.getSentence(), true);    // report input
            newTasks.add(task);       // wait to be processed in the next cycle
            novelTasks.refresh();           // refresh display
        } else {
        	
            //Record.append("!!! Ignored: " + task + "\n");
            
        }
    }

    /**
     * Derived task comes from the inference rules.
     * @param task the derived task
     */
    private void derivedTask(Task task) {
        //Record.append("!!! Derived: " + task + "\n");
    	
        if (task.aboveThreshold()) {
            float budget = task.getBudget().summary();
            
//            float minSilent = Center.mainWindow.silentW.value() / 100.0f;
//            
//            if (budget > minSilent) {  // only report significient derived Tasks
//                report(task.getSentence(), false);
//            }
            
            newTasks.add(task);
            novelTasks.refresh();
        } else {
            //Record.append("!!! Ignored: " + task + "\n");
        }
    }

    /**
     * Reporting executed task, and remember the event
     * <p>
     * called from Operator.call only
     * @param task the executed task
     */
    public void executedTask(Task task) {
        //Record.append("!!! Executed: " + task.getSentence() + "\n");
    	
        Goal g = (Goal) task.getSentence();
        Judgment j = new Judgment(g);
        Task newTask = new Task(j, task.getBudget());
        report(newTask.getSentence(), false);
        newTasks.add(newTask);
        novelTasks.refresh();
    }

    /**
     * Activated task coming from MatchingRules.trySolution
     * @param budget The budget value of the new Task
     * @param sentence The content of the new Task
     * @param isInput Whether the question is input
     */
    public void activatedTask(BudgetValue budget, Sentence sentence, boolean isInput) {
        Task task = new Task(sentence, budget);
        //Record.append("!!! Activated: " + task.toString() + "\n");
        if (sentence instanceof Question) {
            float s = task.getBudget().summary();
//            @SuppressWarnings("static-access")
//            float minSilent = Center.mainWindow.silentW.value() / 100.0f;
//            if (s > minSilent) {  // only report significient derived Tasks
//                report(task.getSentence(), false);
//            }
        }
        newTasks.add(task);
        novelTasks.refresh();
    }

    /* --------------- new task building --------------- */
    /**
     * Shared final operations by all double-premise rules, called from the rules except StructuralRules
     * @param budget The budget value of the new task
     * @param content The content of the new task
     * @param truth The truth value of the new task
     */
    public void doublePremiseTask(BudgetValue budget, Term content, NARTruth truth) {
        if (content != null) {
            Sentence newSentence = Sentence.make(currentTask.getSentence(), content, truth, currentStamp, currentTense);
            Task newTask = new Task(newSentence, budget);
            derivedTask(newTask);
        }
    }

    /**
     * Shared final operations by all single-premise rules, called in StructuralRules
     * @param budget The budget value of the new task
     * @param content The content of the new task
     * @param truth The truth value of the new task
     */
    public void singlePremiseTask(BudgetValue budget, Term content, NARTruth truth) {
        Sentence sentence = currentTask.getSentence();
        Sentence newSentence = Sentence.make(sentence, content, truth, sentence.getStamp(), sentence.getTense());
        Task newTask = new Task(newSentence, budget);
        newTask.setStructual();
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
        Term content = currentTask.getContent();
        TemporalValue tense = currentBelief.getTense();
        Stamp stamp = currentBelief.getStamp();
        Sentence newJudgment = Sentence.make(content, Symbols.JUDGMENT_MARK, truth, stamp, tense);
        Task newTask = new Task(newJudgment, budget);
        newTask.setStructual();
        derivedTask(newTask);
    }

    /* ---------- system working cycle ---------- */
    /**
     * An atomic working cycle of the system: process new Tasks, then fire a concept
     * <p>
     * Called from Center.tick only
     * @param now 
     * @param params 
     */
	public void cycle(long now) {
        // keep the following order
        processTask(now);
        processConcept();
    }

    /**
     * Process the newTasks accumulated in the previous cycle, accept input ones
     * and those that corresponding to existing concepts, plus one from the buffer.
     * @param now 
     * @param params 
     */
    private void processTask(long now) {
        Task task;
        int counter = newTasks.size();  // don't include new tasks produced in the current cycle
        while (counter-- > 0) {
            task = newTasks.remove(0);
            if (task.getSentence().isInput() || (termToConcept(task.getContent()) != null)) { // new input or existing concept
                immediateProcess(task, now);
                if (task.getSentence().isInput() && (task.getSentence() instanceof Question)) {
                	
//                    Concept concept = nameToConcept(task.getSentence().getContent().getID());
//                    if (concept != null) {
//                        concept.startPlay(false);
//                    }
                    
                }
            } else {
                novelTasks.putIn(task);    // delayed processing
            }
        }
        task = novelTasks.takeOut();       // select a task from novelTasks
        if (task != null) {
            immediateProcess(task, now);
        }
    }

    /**
     * Select a concept to fire.
     */
    private void processConcept() {
        Concept currentConcept = concepts.takeOut();
        if (currentConcept != null) {
            currentTerm = currentConcept.getTerm();
            
            //Record.append(" * Selected Concept: " + currentTerm + "\n");
            
            concepts.putBack(currentConcept);   // current Concept remains in the bag all the time
            currentConcept.fire(this);              // a working cycle
            novelTasks.refresh();          // show new result
        }
    }

    /* ---------- task processing ---------- */
    /**
     * Imediate processing of a new task, in constant time
     * Local processing, in one concept only
     * @param task the task to be accepted
     * @param clock 
     * @param params 
     */
    private void immediateProcess(Task task, long now) {
        currentTask = task;
        
        //Record.append("!!! Insert: " + task + "\n");
        
        Term content = task.getContent();
        Concept c = getConcept(content);
        c.directProcess(task, now, getParams());
        if (task.aboveThreshold(getParams().getBudgetThreshold())) {    // still need to be processed
            continuedProcess(task, content);
            Sentence s = task.getSentence();
            if (s.isJudgment() && (s.getTense() != null)) {
                eventProcessing(task);
            }
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
    private void continuedProcess(Task task, Term content) {
        TaskLink taskLink;
        Concept contentConcept = getConcept(content);
        BudgetValue budget = task.getBudget();
        taskLink = new TaskLink(task, null, budget, getParams().getTermLinkRecordLength());   // link type: SELF
        
        contentConcept.insertTaskLink(taskLink);
        activateConcept(contentConcept, budget);       // activate the concept

        
        if (content instanceof CompoundTerm) {
            ArrayList<TermLink> termLinks = (contentConcept != null) ? contentConcept.getTermLinkTemplates()
                    : ((CompoundTerm) content).prepareComponentLinks();  // use saved if exist
            if (termLinks.size() > 0) {
                BudgetValue subBudget = BudgetFunctions.distributeAmongLinks(budget, termLinks.size());
                if (subBudget.aboveThreshold(getParams().getBudgetThreshold())) {
                    Term componentTerm;
                    Concept componentConcept;
                    for (TermLink termLink : termLinks) {
                        if (!(task.isStructual() && (termLink.getType() == TermLink.TRANSFORM))) { // avoid circular transform
                            taskLink = new TaskLink(task, termLink, subBudget, getParams().getTermLinkRecordLength());
                            componentTerm = termLink.getTarget();
                            componentConcept = getConcept(componentTerm);
                            componentConcept.insertTaskLink(taskLink);
                        }
                    }
                    contentConcept.buildTermLinks(this, budget);  // recursively insert TermLink
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
    private void eventProcessing(Task event1) {
        Task event2 = recentEvents.takeOut();
        if ((event2 != null) && event1.getSentence().noOverlapping(event2.getSentence())) {
            long time1 = event1.getSentence().getEventTime();
            long time2 = event2.getSentence().getEventTime();
            TemporalValue order = new TemporalValue((int) (time2 - time1));
            SyllogisticRules.temporalIndCom(event1, event2, order);
            recentEvents.putBack(event2);
        }
        recentEvents.putIn(event1);
    }

//    /* ---------- display ---------- */
//    /**
//     * Display active concepts, called from MainWindow.
//     * @param s the window title
//     */
//    public void conceptsStartPlay(String s) {
//        concepts.startPlay(s);
//    }

//    /**
//     * Display newd tasks, called from MainWindow.
//     * @param s the window title
//     */
//    public void newTasksStartPlay(String s) {
//        novelTasks.startPlay(s);
//        recentEvents.startPlay("Recent Events");
//    }

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
    public void report(Sentence sentence, boolean input) {
        String s;
        if (input) {
            s = "  IN: ";
        } else {
            s = " OUT: ";
        }
        //s += sentence.toString2(this) + "\n";
        s += sentence.toString() + "\n";
        
        //Center.mainWindow.post(s);
    }

    public ConceptBag getConcepts() {
		return concepts;
	}
    public ArrayList<Task> getNewTasks() {
		return newTasks;
	}
    public TaskBuffer getRecentEvents() {
		return recentEvents;
	}

	public void notice(Object message) {
		System.out.println(message);
	}
    
}
