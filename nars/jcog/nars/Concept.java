/*
 * Concept.java
 *
 * Copyright (C) 2008  Pei Wang
 *
 * This file is part of Open-NARS.
 *
 * Open-NARS is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * Open-NARS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Open-NARS.  If not, see <http://www.gnu.org/licenses/>.
 */
package jcog.nars;

import java.util.ArrayList;





import org.opencog.reason.nars.Memory;
import org.opencog.reason.nars.NARParams;
import org.opencog.reason.nars.inference.BudgetFunctions;
import org.opencog.reason.nars.inference.MatchingRules;
import org.opencog.reason.nars.inference.RuleTables;
import org.opencog.reason.nars.inference.TemporalRules;
import org.opencog.reason.nars.inference.UtilityFunctions;
import org.opencog.reason.nars.language.CompoundTerm;
import org.opencog.reason.nars.language.Inheritance;
import org.opencog.reason.nars.language.Term;
import org.opencog.reason.nars.operation.Operator;
import org.opencog.reason.nars.store.TaskLinkBag;
import org.opencog.reason.nars.store.TermLinkBag;


/**
 * A concept contains information associated with a term, including directly 
 * and indirectly related tasks and beliefs.
 * <p>
 * To make sure the space will be released, the only allowed reference to a concept are
 * those in a ConceptBag. All other access go through the Term that names the concept.
 */
public final class Concept extends Item {

    /** The term is the unique ID of the concept */
    private Term term;
    
    /** Task links for indirect processing */
    private TaskLinkBag taskLinks;
    
    /** Term links between the term and its components and compounds */
    private TermLinkBag termLinks;
    
    /** Link templates of TermLink, only in concepts with CompoundTerm */
    private ArrayList<TermLink> termLinkTemplates;
    
    /** Question directly asked about the term */
    private ArrayList<Question> pendingQuestions;
    
    /** Goals directly requested on the term */
    private ArrayList<Goal> pendingGoals;
    
    /** Judgments directly made about the term, with non-future tense */
    private ArrayList<Judgment> pastRecords;
    
    /** Judgments directly made about the term, with future tense */
    private ArrayList<Judgment> predictions; // event only
    
    /** Most recent and confidence judgment */
    private Judgment presentBelief = null;

    /** Whether truth value of judgments can be revised */
    private boolean revisible = true;               //
    


    /* ---------- constructor and intialization ---------- */
    /**
     * Constructor, called in Memory.getConcept only
     * @param params 
     * @param tm A term corresponding to the concept
     */
    public Concept(NARParams params, Term tm) {
        super();
        key = tm.toString();
        term = tm;
        pendingQuestions = new ArrayList<Question>();
        pendingGoals = new ArrayList<Goal>();
        pastRecords = new ArrayList<Judgment>();
        predictions = new ArrayList<Judgment>();
        taskLinks = new TaskLinkBag(params);
        termLinks = new TermLinkBag(params);
        if (tm instanceof CompoundTerm) {
            termLinkTemplates = ((CompoundTerm) tm).prepareComponentLinks();
            checkRevisibility();
//            if (tm instanceof Temporal) {
//                markEventComponents();
//            }
        }
    }

    /**
     * Judgments with dependent variable cannot be revised
     */
    private void checkRevisibility() {
        revisible = ((key.indexOf("()") < 0) && (key.indexOf("(#")) < 0);
    }

    /* ---------- direct processing of tasks ---------- */
    /**
     * Directly process a new task. Called exactly once on each task.
     * Using local information and finishing in a constant time.
     * Provide feedback in the budget value of the task.
     * <p>
     * called in Memory.immediateProcess only
     * @param task The task to be processed
     * @param now 
     * @param params 
     */
    public void directProcess(Task task, long now, NARParams params) {
        Sentence sentence = task.getSentence();
        if (sentence instanceof Question) {
            processQuestion((Question) sentence, task);
        } else if (sentence instanceof Goal) {
            processGoal((Goal) sentence, task, params);
        } else {
            processJudgment((Judgment) sentence, task, now, params);
        }
//        if (showing) {
//            window.post(displayContent());  // show changes
//        }
    }

    /**
     * To answer a question by existing beliefs
     * @param ques The question to be answered
     * @param task The task to be processed
     */
    private void processQuestion(Question ques, Task task) {
        boolean duplicate = false;
        for (Question q : pendingQuestions) {           // keep the existing one for answer information
            if (q.equals(ques)) {
                duplicate = true;
            }
        }
        if (!duplicate) {
            pendingQuestions.add(ques);
        }
        if ((ques.temporalOrder == null) || ques.temporalOrder.getDelta() < 0) {
            for (Judgment judg : pastRecords) {
                MatchingRules.trySolution(ques, judg, task);    // look for better answer
            }
        } else if (ques.temporalOrder.getDelta() > 0) {
            for (Judgment judg : predictions) {
                MatchingRules.trySolution(ques, judg, task);    // look for better answer
            }
        } else if (presentBelief != null) {
            MatchingRules.trySolution(ques, presentBelief, task);
        }
    }

    /**
     * Direct processing a new goal
     * @param goal The goal to be processed
     * @param task The task to be processed
     * @param params 
     */
    private void processGoal(Goal goal, Task task, NARParams params) {
        boolean revised = false;
        if (revisible) {
            revised = reviseTable(goal, task, pendingGoals);     // revise desire
            if (revised) {      // don't process this goal, but the revised version
                return;
            }
        }
        if (presentBelief != null) {
            MatchingRules.trySolution(goal, presentBelief, task);   // reality check
        }
        Term content = goal.getContent();
        if ((task.getPriority() >= params.getPriorityThreshold()) && (content instanceof Inheritance)) {
            Term pred = ((Inheritance) content).getPredicate();
            if (pred instanceof Operator) {
                float netDesire = goal.getTruth().getExpectation();
                if (netDesire > params.getDecisionThreshold()) {
                    ((Operator) pred).call(task);
                    task.setPriority(0.0f);        // each call is executed once
                    return;
                }
            }
        }
        if (task.aboveThreshold()) {
            addToTable(goal, pendingGoals, params.getMaximumGoalsLength());   // indirectly archiving
            Question ques = new Question(goal);
            Memory.activatedTask(task.getBudget(), ques, false);
        }
    }

    /**
     * To accept a new judgment as belief, and check for revisions and solutions
     * @param judg The judgment to be accepted
     * @param task The task to be processed
     * @param now 
     * @param params 
     */
    private void processJudgment(Judgment judg, Task task, long now, NARParams params) {
        if (revisible) {
            boolean revised;
            if (judg.isFuture(now)) {
                revised = reviseTable(judg, task, predictions, now, params);
            } else {
                revised = reviseTable(judg, task, pastRecords, now, params);
            }
            if (!revised) {
                tryUpdate(judg, now);
            }
        }
        if (task.aboveThreshold(params.getBudgetThreshold())) {
            for (Question ques : pendingQuestions) {
                MatchingRules.trySolution(ques, judg, task);
            }
            for (Goal goal : pendingGoals) {
                MatchingRules.trySolution(goal, judg, task);
            }
            
            int maxBeliefLength = params.getMaximumBeliefLength();
            if (judg.isFuture(now)) {
                addToTable(judg, predictions, maxBeliefLength);
            } else {
                addToTable(judg, pastRecords, maxBeliefLength);
            }
        }
    }

    /**
     * Revise existing beliefs or goals
     * @param task The task to be processed
     * @param table The table to be revised
     * @param params 
     * @return Whether the new belief triggered a temporalRevision
     */
    private boolean reviseTable(Judgment newSentence, Task task, ArrayList table, long now, NARParams params) {
        boolean revised = false;
        Judgment belief;
        for (int i = table.size() - 1; i >= 0; i--) {
            belief = (Judgment) table.get(i);
            if ((table == predictions) && !newSentence.isFuture(now)) {
                table.remove(i);
                addToTable(belief, pastRecords, params.getMaximumBeliefLength());
            } else if (belief.noOverlapping(newSentence)) {
                revised |= MatchingRules.revision(task, belief, false);
            }
        }
        return revised;
    }

    /**
     * To temporalRevision the presentBelief
     * @param judg The new belief
     * @param now 
     */
    private void tryUpdate(Judgment judg, long now) {
        if (presentBelief == null) {
            presentBelief = judg;
        } else {    // presentBelief != null
            if (presentBelief.getTense() == null) {
                if (judg.getTense() == null) {
                    if (presentBelief.getTruth().getConfidence() < judg.getTruth().getConfidence()) {
                        presentBelief = judg;
                    }
                } else {    // judg.getTense() != null
                    if (judg.getEventTime() == now) {
                        presentBelief = judg;
                    }
                }
            } else {    // presentBelief.getTense() != null
                if (judg.getTense() != null) {
                    if (judg.getEventTime() < presentBelief.getEventTime()) {
                        return;
                    }
                    if (judg.getTruth().getExpDifAbs(presentBelief.getTruth()) > 0.5) {
                        presentBelief = judg;
                    } else if (judg.noOverlapping(presentBelief)) {
                        TemporalRules.temporalRevision(judg, presentBelief, now, false);
                    } else if (presentBelief.getTruth().getConfidence() < judg.getTruth().getConfidence()) {
                        presentBelief = judg;
                    }
                }
            }
        }
    }

    /**
     * Add a new belief or goal into the table
     * Sort the beliefs/goals by rank, and remove redundant or low rank one
     * @param newJudgment The judgment to be processed
     * @param table The table to be revised
     * @param capacity The capacity of the table
     */
    @SuppressWarnings("unchecked")
    private void addToTable(Judgment newJudgment, ArrayList table, int capacity) {
        float rank1 = BudgetFunctions.rankBelief(newJudgment);    // for the new belief
        Judgment judgment2;
        float rank2;
        int i;
        for (i = 0; i < table.size(); i++) {        // go through everyone
            judgment2 = (Judgment) table.get(i);
            rank2 = BudgetFunctions.rankBelief(judgment2); // previous belief
            if (rank1 >= rank2) {
                if (newJudgment.equivalentTo(judgment2)) {
                    return;
                }
                table.add(i, newJudgment);
                break;
            }
        }
        if (table.size() >= capacity) {
           while (table.size() > capacity)
               table.remove(table.size() - 1);
        } else if (i == table.size()) {
            table.add(newJudgment);
        }
    }

    /* ---------- insert Links for indirect processing ---------- */
    /**
     * Insert a TaskLink into the TaskLink bag
     * <p>
     * called only from Memory.continuedProcess
     * @param taskLink The termLink to be inserted
     */
    public void insertTaskLink(TaskLink taskLink) {
        BudgetValue budget = taskLink.getBudget();
        taskLinks.putIn(taskLink);
    }

    /**
     * Recursively build TermLinks between a compound and its components
     * <p>
     * called only from Memory.continuedProcess
     * @param budget The budget of the task
     */
    public void buildTermLinks(Memory memory, BudgetValue budget) {
        Term t;
        Concept concept;
        TermLink termLink1, termLink2;
        if (termLinkTemplates.size() > 0) {
            BudgetValue subBudget = BudgetFunctions.distributeAmongLinks(budget, termLinkTemplates.size());
            
            float budgetThreshold = memory.getParams().getBudgetThreshold();
            
			if (subBudget.aboveThreshold(budgetThreshold)) {
                for (TermLink template : termLinkTemplates) {
                    if (template.getType() != TermLink.TRANSFORM) {
                        t = template.getTarget();
                        concept = memory.getConcept(t);
                        termLink1 = new TermLink(t, template, subBudget);
                        insertTermLink(termLink1);   // this termLink to that
                        termLink2 = new TermLink(term, template, subBudget);
                        concept.insertTermLink(termLink2);   // that termLink to this
                        if (t instanceof CompoundTerm) {
                            concept.buildTermLinks(memory, subBudget);
                        }
                    }
                }
            }
        }
    }

    /**
     * Insert a TermLink into the TermLink bag
     * <p>
     * called from buildTermLinks only
     * @param termLink The termLink to be inserted
     */
    public void insertTermLink(TermLink termLink) {
        termLinks.putIn(termLink);
    }

    /* ---------- access local information ---------- */
    /**
     * Return the assocated term, called from Memory only
     * @return The assocated term
     */
    public Term getTerm() {
        return term;
    }

    /**
     * Return a string representation of the concept, called in ConceptBag only
     * @return The concept name, with budget in the full version
     */
    @Override
    public String toString() {  // called from concept bag
//        if (NARS.isStandAlone()) {
//            return (super.toString2() + " " + key);
//        } else {
            return key;
//        }
    }

    /**
     * Recalculate the quality of the concept [to be refined]
     * @return The quality value
     */
    @Override
    public float getQuality() {
        float linkPriority = termLinks.averagePriority();
        float termComplexityFactor = 1.0f / term.getComplexity();
        return UtilityFunctions.or(linkPriority, termComplexityFactor);
    }

    /**
     * Return the templates for TermLinks, only called in Memory.continuedProcess
     * @return The template get
     */
    public ArrayList<TermLink> getTermLinkTemplates() {
        return termLinkTemplates;
    }

    /**
     * Select a belief to interact with the given task in inference
     * <p>
     * get the first qualified one
     * <p>
     * only called in RuleTables.reason
     * @param task The selected task
     * @return The selected belief
     */
    public Judgment getBelief(Task task) {
        Sentence taskSentence = task.getSentence();
        Judgment belief;
        for (int i = 0; i < pastRecords.size(); i++) {
            belief = pastRecords.get(i);
            if (belief.noOverlapping(taskSentence)) {
                //Record.append(" * Selected Belief: " + belief + "\n");
                return belief;
            }
        }
        return null;
    }

    /* ---------- main loop ---------- */
    /**
     * An atomic step in a concept, only called in Memory.processConcept
     * @param memory 
     */
    public void fire(Memory memory) {
        TaskLink tLink = taskLinks.takeOut();
        if (tLink == null) {
            return;
        }
        memory.currentTaskLink = tLink;
        memory.currentBeliefLink = null;
        
        memory.notice(" * Selected TaskLink: " + tLink + "\n");
        
        Task task = tLink.getTargetTask();
        memory.currentTask = task;
        if (tLink.getType() == TermLink.TRANSFORM) {
            RuleTables.transformTask(task, tLink);  // to turn this into structural inference as below?
            return;
        }
        
        int termLinkCount = memory.getParams().getMaxReasonedTermLink(); 
        
        while (memory.noResult() && (termLinkCount > 0)) {
            TermLink termLink = termLinks.takeOut(tLink);
            if (termLink != null) {
                memory.notice(" * Selected TermLink: " + termLink + "\n");
                
                memory.currentBeliefLink = termLink;
                
                RuleTables.reason(tLink, termLink);
                termLinks.putBack(termLink);
                termLinkCount--;
            } else {
                termLinkCount = 0;
            }
        }
        taskLinks.putBack(tLink);
    }

    /* ---------- display ---------- */
//    /**
//     * Start displaying contents and links, called from ConceptWindow or Memory.processTask only
//     * @param showLinks Whether to display the task links
//     */
//    public void startPlay(boolean showLinks) {
//    	if (window != null && window.isVisible()){
//    		window.detachFromConcept();
//    	}
//  		window = new ConceptWindow(this);
//        showing = true;
//        window.post(displayContent());
//        if (showLinks) {
//            taskLinks.startPlay("Task Links in " + term);
//            termLinks.startPlay("Term Links in " + term);
//        }
//    }
//
//    /**
//     * Resume display, called from ConceptWindow only
//     */
//    public void play() {
//        showing = true;
//        window.post(displayContent());
//    }

//    /**
//     * Stop display, called from ConceptWindow only
//     */
//    public void stop() {
//        showing = false;
//    }

    /**
     * Collect direct belief, questions, and goals for display
     * @return String representation of direct content
     */
    private String displayContent() {
        StringBuffer buffer = new StringBuffer();
        if (presentBelief != null) {
            buffer.append("  Present Belief:\n");
            buffer.append(presentBelief + "\n");
        }
        if (pastRecords.size() > 0) {
            buffer.append("\n  Beliefs:\n");
            for (Sentence s : pastRecords) {
                buffer.append(s + "\n");
            }
        }
        if (predictions.size() > 0) {
            buffer.append("\n  Predictions:\n");
            for (Sentence s : predictions) {
                buffer.append(s + "\n");
            }
        }
        if (pendingGoals.size() > 0) {
            buffer.append("\n  Goals:\n");
            for (Sentence s : pendingGoals) {
                buffer.append(s + "\n");
            }
        }
        if (pendingQuestions.size() > 0) {
            buffer.append("\n  Question:\n");
            for (Sentence s : pendingQuestions) {
                buffer.append(s + "\n");
            }
        }
        return buffer.toString();
    }
}

