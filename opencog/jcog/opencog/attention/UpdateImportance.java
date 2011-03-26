package jcog.opencog.attention;

import java.util.List;
import java.util.logging.Logger;
import jcog.opencog.Atom;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import jcog.opencog.atom.AttentionValue;

/** ImportantUpdatingAgent updates the AttentionValues of atoms.
 * 
 * This Agent carries out:
 *   - stimulus to STI and LTI conversion
 *   - rent collection
 *   - rent adjustment and taxing when AtomSpace funds go out of
 *     homeostatic bounds.
 *
 * @see http://www.opencog.org/wiki/ImportanceUpdatingAgent
 *
 * As atoms are used in mind agents they are endowed with stimulus.
 * Agents should explicit grant atoms stimulus when they make use of them,
 * although it's left up to the Agent to decide how best to do this.
 *
 * The ImportanceUpdatingAgent takes this stimulus and pays wages to atoms. So
 * in a sense, the stimulus is a record of how much work the atom has been
 * involved in. The currency of the wages that the ImportanceUpdatingAgent pays
 * to an atom is in the form of both STI and LTI. The amount of STI and LTI
 * conferred is based both on the total stimulus of the atom, and two internal
 * multipliers that indicate the rate that stimulus is converted into STI and
 * LTI. If an Agent has insufficient funds to pay at these rates, the Agent's 
 * available funds is divided proportionately among that Atoms it has 
 * stimulated.  As such, the global multipliers for stimulus act as a "cap", so
 * Agents need not go broke providing stimulus. Atoms also have to pay LTI rent 
 * to exist in the the AtomSpace and STI rent to exist in the Attentional focus.
 *
 * STI rent is charged from atoms that are above the Attentional focus boundary
 * and LTI rent for all atoms in the system (conceptually at least, once atoms
 * start being swapped to disk, LTI rent for these atoms might only be charged
 * periodically due to the expense involved in updating them). So, if the
 * attentional focus boundary was set at zero, i.e. all atoms with positive STI
 * are in the attention span of the OpenCog instance, then for our atom A1, it
 * would be charged both STI and LTI rent. If STI was < 0 then atom A1 would not
 * be charged STI rent, but would still be charged LTI rent regardless of the
 * STI or LTI of A1. 
 *
 * Since both STI and LTI currency are conserved, the funds that the
 * ImportanceUpdatingAgent uses to pay wages has to come from somewhere. In an
 * ideal situation, this would be balanced by the rent charged to atoms.
 * However, the number of atoms in the AtomSpace and in the attentional focus
 * will be constantly changing. In fact, an OpenCog instance may even have some
 * method of controlling the attentional focus boundary depending on how
 * focussed or quick thought and reaction needs to be. more atoms in the
 * attentional focus mean more atoms to consider during reasoning, although
 * reasoning methods can and do use atoms outside of the attentional focus if
 * the those in attention are insufficient to come up with suitable results. To
 * manage the variable number of atoms in attention and in the Atom Space, the
 * ImportanceUpdatingAgent draws STI and LTI from a pool of funds managed by the
 * Atom Space. These pools have a homeostatic range that the
 * ImportanceUpdatingAgent tries to keep the Atom Space funds within. If, at the
 * end of an update cycle, the pools are outside of this range, then the agent
 * taxes all atoms to bring the pools back to the middle of the range (Note that
 * although it's called a tax, it can actually result in a refund if the agent
 * has been charging too much tax... just like real life ;-) ). The agent then
 * recalculates the optimal rent based on decaying measures of the AtomSpace
 * size and number of atoms in the attentional focus.
 *
 * @todo Remove the conversion of stimulus to STI/LTI from
 * ImportanceUpdatingAgent. Create a opencog::MindAgent function that converts stimulus
 * into STI/LTI as well as resetting the stimulus map. This function should be
 * called at the end of the MindAgent run method... but individual MindAgents
 * may choose not to (if they were completely unsuccessful and don't want to
 * give any funds to the atoms).
 */
public class UpdateImportance extends MindAgent {

    private OCMind mind;
    private Logger logger;
    private boolean initialEstimateMade = false;
    private boolean noiseEnabled;
    private boolean lobeSTIOutOfBounds;
    int totalStimulus = 0;

    public UpdateImportance() {
        super();

        this.logger = Logger.getLogger(this.toString());
    }

    public OCMind getMind() {
        return mind;
    }

    public Logger getLogger() {
        return logger;
    }

    protected void init() {
    }

    public boolean isNoiseEnabled() {
        return noiseEnabled;
    }

    public void setNoiseEnabled(boolean noiseEnabled) {
        this.noiseEnabled = noiseEnabled;
    }

    @Override
    public synchronized void run(OCMind mind) {
        this.mind = mind;

        List<MindAgent> agents = getMind().getAgents();

        //HandleEntry *h, *q;

        short maxSTISeen = AttentionValue.MinSTI;
        short minSTISeen = AttentionValue.MaxSTI;

        getLogger().fine("ImportanceUpdatingAgent.run");

        /* init iterative variables, that can't be calculated in */
        if (!initialEstimateMade) {
            init();
        }

        /* Calculate attentional focus sizes */
        updateAttentionalFocusSizes();

        /* Random stimulation if on */
        if (isNoiseEnabled()) {

            //log->debug("Random stimulation on, stimulating atoms");
            for (MindAgent ag : agents) {
                randomStimulation(ag);
            }

        }

        /* Update stimulus totals */
        updateTotalStimulus(agents);

        /* Update atoms: Collect rent, pay wages */

        /* Calculate STI/LTI atom wages for each agent */
        calculateAtomWages(agents);

        for (MindAgent agent : agents) {            
            for (Atom at : agent.getStimulated()) {
                updateAtomSTI(agents, at);
                updateAtomLTI(agents, at);

                /* Enfore sti and lti caps */
                enforceSTICap(at);
                enforceLTICap(at);

                short sti = mind.getAttention(at).getSTI();

                // Greater than max sti seen?
                if (sti > maxSTISeen) {
                    maxSTISeen = sti;
                }
                if (sti < minSTISeen) {
                    minSTISeen = sti;
                }
            }
        }


        // Update AtomSpace recent maxSTI and recent minSTI
        if (minSTISeen > maxSTISeen) {
            // if all Atoms have the same STI this will occur
            minSTISeen = maxSTISeen;
        }

        getMind().setSTIRange(minSTISeen, maxSTISeen);

        //	    getLogger().fine("Max STI seen is " + maxSTISeen + " recentMaxSTI is now ??" /*+ a->getMaxSTI()*/);
        //	    getLogger().fine("Min STI seen is " + minSTISeen + " recentMinSTI is now ??" /*+ a->getMinSTI()*/);

        /* Check AtomSpace funds are within bounds */
        checkAtomSpaceFunds();

        if (lobeSTIOutOfBounds) {
            getLogger().fine("Lobe STI was out of bounds, updating STI rent");
            updateSTIRent();
        }
        /* Not sure whether LTI rent should be updated */
        //if (lobeLTIOutOfBounds) {
        //    log->debug("Lobe LTI was out of bounds, updating LTI rent");
        //    updateLTIRent(a);
        //}

        /* Reset Stimulus */
        for (MindAgent ag : agents) {
            ag.resetStimulus();
        }
    }

    private void enforceLTICap(Atom at) {
    }

    private void enforceSTICap(Atom at) {
    }

    private void updateAtomLTI(List<MindAgent> agents, Atom at) {
    }

    private void updateAtomSTI(List<MindAgent> agents, Atom a) {
        //This is a hack which converts stimulus to attention directly
        short ts = 0;
        for (MindAgent m : agents) {
            ts += m.getStimulus(a);
        }
        mind.getAttention(a).addSTI(ts); 
        
//        AttentionValue::sti_t current, exchangeAmount;
//        current = a->getAttentionBank().getSTI(agent);
//
//        /* TODO
//         *
//         * The wikibook says:
//         *
//         *    Currency flows from Units to MindAgents when Units reward MindAgents 
//         *    for helping achieve system goals.
//         *
//         *    Currency flows from MindAgents to Units when MindAgents pay Units 
//         *    rent for the processor time they utilize.
//         */
//        exchangeAmount = 0;
//
//        // just top up to a fixed amount for now
//        if (current < STIAtomWage * 100)
//            exchangeAmount = STIAtomWage * 100 - current;
//
//        a->getAttentionBank().setSTI(agent, current + exchangeAmount);    
        
    }

    private void updateSTIRent() {
    }

    private void checkAtomSpaceFunds() {
    }

    private void calculateAtomWages(List<MindAgent> agents) {
    }


    private void updateTotalStimulus(List<MindAgent> agents) {
        totalStimulus = 0;
        for (MindAgent a : agents) {
            totalStimulus += a.getTotalStimulus();
        }
        //totalStimulusSinceReset.update(total);
    }

    private void randomStimulation(MindAgent ag) {
//		int expectedNum, actualNum;
//		HandleEntry *h, *q;
//		opencog::RandGen *rng;
//
//		rng = getRandGen();
//
//		// TODO: use util::lazy_random_selector and a binomial dist
//		// to get actualNum
//		actualNum = 0;
//
//		h = getHandlesToUpdate(a);
//		expectedNum = (int) (noiseOdds * h->getSize());
//
//		q = h;
//		while (q) {
//			double r;
//			r = rng->randdouble();
//			if (r < noiseOdds) {
//				agent->stimulateAtom(q->handle, noiseUnit);
//				actualNum++;
//			}
//			q = q->next;
//		}
//
//		log->info("Applied stimulation randomly to %d " \
//				"atoms, expected about %d.", actualNum, expectedNum);
//
//		delete h;
    }

    protected void updateAttentionalFocusSizes() {
//		int n = 0;
//		HandleEntry* inFocus;
//		HandleEntry* h;
//
//		const AtomTable& at = a->getAtomTable();
//		inFocus = at.getHandleSet(a->getAttentionalFocusBoundary() + amnesty, AttentionValue::MAXSTI);
//
//		attentionalFocusSize.update(inFocus->getSize());
//
//		log->fine("attentionalFocusSize = %d, recent = %f",
//				attentionalFocusSize.val, attentionalFocusSize.recent);
//
//		h = inFocus;
//		while (h) {
//			if (a->isNode(h->getAtom()->getType()))
//				n += 1;
//			h = h->next;
//		}
//		attentionalFocusNodesSize.update(n);
//
//		log->fine("attentionalFocusNodesSize = %d, recent = %f",
//				attentionalFocusNodesSize.val, attentionalFocusNodesSize.recent);
//
//		delete inFocus;
    }

    public void log() {
        //	    s << "Importance Updating Mind Agent\n";
        //	    s << "STIAtomRent: " << STIAtomRent << "\n";
        //	    s << "STIAtomWage: " << STIAtomWage << "\n";
        //	    s << "LTIAtomRent: " << LTIAtomRent << "\n";
        //	    s << "LTIAtomWage: " << LTIAtomWage << "\n";
        //	    s << "AV Caps (STI/LTI): " << STICap << "/" << LTICap << "\n";
        //	    s << "Updating Links: ";
        //	    if (updateLinks) s <<  "Yes";
        //	    else s << "No";
        //	    s << "\n";
        //	    if (noiseOn)
        //	        s << "Random stimulation on. Chance: " << noiseOdds << \
        //	        " Amount: " << noiseUnit << "\n";
        //	    s << "Recent Total Stim since reset: " << totalStimulusSinceReset.recent \
        //	    << ", decay: " << totalStimulusSinceReset.decay << "\n";
        //	    s << "Att. focus. Size: " << attentionalFocusSize.val << ", recent: " \
        //	    << attentionalFocusSize.recent << ", recentForNodes: " \
        //	    << attentionalFocusNodesSize.val << ", decay: " \
        //	    << attentionalFocusSize.decay << "\n";
        //	    s << "target (range) STI: " << targetLobeSTI << \
        //	    "(" << acceptableLobeSTIRange[0] << "-" << acceptableLobeSTIRange[1] << \
        //	    ") LTI: " << targetLobeLTI << \
        //	    "(" << acceptableLobeLTIRange[0] << "-" << acceptableLobeLTIRange[1] << \
        //	    ")\n";
        //
        //	    s.put(0); //null terminate the string cout
        //	    return s.str();
    }
    //    friend class ::ImportanceUpdatingAgentUTest;
    //
    //    public:
    //        
    //        /** The different ways rent can be calculated 
    //         * for atoms in the attentional focus.
    //         */
    //        enum rentType_t {
    //            RENT_FLAT, //!< Use a flat rent 
    //            RENT_EXP, //!< Use a exponential rent
    //            RENT_LOG //!< Use a logarithmic rent
    //        };
    //
    //    private:
    //
    //        AttentionValue::sti_t STIAtomRent; //!< Current atom STI rent.
    //        opencog::recent_val<AttentionValue::sti_t> STITransitionalAtomRent; //!< Decaying rent
    //        AttentionValue::lti_t LTIAtomRent; //!< Current atom LTI rent.
    //        
    //        AttentionValue::sti_t amnesty; //!< Amnesty is used in calculating rent.
    //
    //        enum rentType_t rentType; //!< Current method for calculating rent.
    //
    //        //! Rent function parameters that tune rent equations.
    //        std::vector<double> rentFunctionParams;
    //
    //        /** Calculate the rent to apply to an atom with sti \a c.
    //         *
    //         * @param a The AtomSpace to work on.
    //         * @param c The STI of the atom to calculate rent for.
    //         */
    //        AttentionValue::sti_t calculateSTIRent(AtomSpace* a, AttentionValue::sti_t c);
    //        
    //        AttentionValue::sti_t STIAtomWage; //!< Max atom STI wage per stimulus
    //        AttentionValue::lti_t LTIAtomWage; //!< Max atom LTI wage per stimulus
    //        std::vector<float> STIAtomWageForAgent; //!< Atom STI wage per stimulus for each Agent
    //        std::vector<float> LTIAtomWageForAgent; //!< Atom LTI wage per stimulus for each Agent
    //
    //        /** Calculate the wages to pay to atoms for each agent
    //         *
    //         * @param a The atom space.
    //         * @param agents The list of running agents.
    //         */
    //        void calculateAtomWages(AtomSpace *a, const AgentSeq &agents);
    //
    //        AttentionValue::sti_t STICap; //!< Cap on STI
    //        AttentionValue::lti_t LTICap; //!< Cap on LTI
    //
    //        bool updateLinks; //!< Update links or not
    //
    //        /**
    //         * Randomly stimulate atoms in the AtomSpace for a given Agent.
    //         * Simulates a "cognitive process" (e.g. a Novamente lobe)
    //         * that stimulates Atoms selectively. May also be useful to
    //         * introduce a level of noise into the system.
    //         *
    //         * @param AtomSpace
    //         * @param Agent to act upon
    //         */
    //        void randomStimulation(AtomSpace *a, Agent *agent);
    //
    //        bool noiseOn;     //!< Randomly stimulate atoms?
    //        float noiseOdds;  //!< Chance of randomly introduced stimulus
    //        stim_t noiseUnit; //!< The default stimulus unit used by random stimulation
    //
    //        //! Recent amount of stimulus given per cycle
    //        opencog::recent_val<stim_t> totalStimulusSinceReset;
    //
    //        //! Recent number of atoms observed within the AtomSpace attention focus.
    //        opencog::recent_val<long> attentionalFocusSize;
    //        //! Recent number of \b nodes observed within the AtomSpace attention focus.
    //        opencog::recent_val<long> attentionalFocusNodesSize;
    //
    //        //! Indicates whether STI has gone out of acceptable range during this run.
    //        bool lobeSTIOutOfBounds;
    //        //! Indicates whether LTI has gone out of acceptable range during this run.
    //        bool lobeLTIOutOfBounds;
    //
    //        /** Collect STI rent for Agents based on processor time they utilize,
    //         * and pay wages based on how well they acheive system goals.
    //         *
    //         * @param a The AtomSpace the Agent is working on.
    //         * @param agent The Agent to update.
    //         */
    //        void updateAgentSTI(AtomSpace* a, Agent *agent);
    //
    //        /** Collect LTI rent for Agents based on processor time they utilize,
    //         * and pay wages based on how well they acheive system goals.
    //         *
    //         * @param a The AtomSpace the Agent is working on.
    //         * @param agent The Agent to update.
    //         */
    //        void updateAgentLTI(AtomSpace* a, Agent *agent);
    //
    //        /** Collect STI rent for atoms within attentional focus 
    //         * and pay wages based on amount of stimulus.
    //         *
    //         * @param a The AtomSpace the Agent is working on.
    //         * @param agents The list of running agents.
    //         * @param h The Handle of the atom to update.
    //         */
    //        void updateAtomSTI(AtomSpace* a, const AgentSeq &agents, Handle h);
    //
    //        /** Collect LTI rent for all atoms and pay wages based on stimulation
    //         *
    //         * @param a The AtomSpace the Agent is working on.
    //         * @param agents The list of running agents.
    //         * @param h The Handle of the atom to update.
    //         */
    //        void updateAtomLTI(AtomSpace* a, const AgentSeq &agents, Handle h);
    //
    //        /** Cap STI values to the maximum to prevent atoms
    //         * becoming all important.
    //         *
    //         * @param a The AtomSpace the Agent is working on.
    //         * @param h The Handle of the atom to update.
    //         * @return Whether any atoms had an STI above the cap.
    //         */
    //        bool enforceSTICap(AtomSpace* a, Handle h);
    //
    //        /** Cap LTI values to the maximum to prevent atoms
    //         * becoming all important.
    //         *
    //         * @param a The AtomSpace the Agent is working on.
    //         * @param h The Handle of the atom to update.
    //         * @return Whether any atoms had an LTI above the cap.
    //         */
    //        bool enforceLTICap(AtomSpace* a, Handle h);
    //
    //        /** Recalculate the STI Rent to charge atoms.
    //         *
    //         * @param a The AtomSpace the Agent is working on.
    //         * @param gradual Change this rent gradually
    //         */
    //        void updateSTIRent(AtomSpace* a, bool gradual = false);
    //
    //    	/** Recalculate the LTI Rent to charge atoms.
    //    	 *
    //         * @param a The AtomSpace the MindAgent is working on.
    //    	 */
    //        void updateLTIRent(AtomSpace* a);
    //
    //        /** Check whether AtomSpace funds are within limits, and make changes
    //         * if not.
    //         *
    //         * @param a The AtomSpace to work in.
    //         * @return Whether the funds were in the homeostatic bounds.
    //         */
    //        bool checkAtomSpaceFunds(AtomSpace* a);
    //
    //        /** Decide whether first value is in the range specified by the 2
    //         * values in range.
    //         *
    //         * @param val value to check.
    //         * @param range interval to check \a val is in.
    //         * @return whether val is within range.
    //         */
    //        bool inRange(long val, long range[2]) const;
    //
    //        /** If STI funds are outside of acceptable limits, then the STI funds
    //         * are adjusted accordingly by applying an AtomSpace wide tax/rebate.
    //         *
    //         * @param a The AtomSpace to work in.
    //         */
    //        void adjustSTIFunds(AtomSpace* a);
    //
    //        /** If LTI funds are outside of acceptable limits, then the LTI funds
    //         * are adjusted accordingly by applying an AtomSpace wide tax/rebate.
    //         *
    //         * @param a The AtomSpace to work in
    //         */
    //        void adjustLTIFunds(AtomSpace* a);
    //
    //        /** Get the amount of tax/rebate to apply based on a mean tax/rebate
    //         * value.
    //         *
    //         * When adjusting funds, use the \a mean value to stochastically
    //         * determine how much rent to charge an atom. (This is because STI/LTI
    //         * are integers and the tax amount will possibly be < 1 in any reasonable
    //         * sized OpenCog instance.)
    //         *
    //         * Internally takes the integer component of the mean, and samples
    //         * from a Poisson distribution for the remainder.
    //         *
    //         * @param mean The mean tax that would be charged if STI/LTI were a float.
    //         * @return An integer amount of tax to charge
    //         */
    //        int getTaxAmount(double mean);
    //
    //        /** Get Random number generator associated with Agent,
    //         * and instantiate if it does not already exist.
    //         *
    //         * @return Pointer to the internal random number generator.
    //         */
    //        opencog::RandGen* getRandGen();
    //        opencog::RandGen* rng; //!< Random number generator pointer.
    //
    //        /** Update the attentional focus size variables needed for
    //         * tuning attention dynamics.
    //         *
    //         * @param a The AtomSpace to work on
    //         */
    //        void updateAttentionalFocusSizes(AtomSpace* a);
    //
    //        /** Initialise iterative variables with suitable starting values.
    //         *
    //         * @param server A pointer to the \a CogServer running the Agent.
    //         */
    //        void init(CogServer *server);
    //
    //        //! Has init been run to give iterative variables sensible start points
    //        bool initialEstimateMade;
    //
    //        /** Update the total stimulus variables.
    //         *
    //         * @param agents The list of running Agents
    //         */
    //        void updateTotalStimulus(const AgentSeq &agents);
    //
    //        /** Gets either all Atoms or all Nodes, depending on \a updateLinks.
    //         *
    //         * @param a The AtomSpace to work on.
    //         * @return The Handles to process/update.
    //         */
    //        HandleEntry* getHandlesToUpdate(AtomSpace* a);
    //
    //        /** Set the agent's logger object
    //         *
    //         * Note, this will be deleted when this agent is.
    //         *
    //         * @param l The logger to associate with the agent.
    //         */
    //        void setLogger(Logger* l);
    //
    //        Logger *log; //!< Logger object for Agent
    //
    //    public:
    //
    //        virtual const ClassInfo& classinfo() const { return info(); }
    //        static const ClassInfo& info() {
    //            static const ClassInfo _ci("opencog::ImportanceUpdatingAgent");
    //            return _ci;
    //        }
    //
    //        ImportanceUpdatingAgent();
    //        virtual ~ImportanceUpdatingAgent();
    //        virtual void run(CogServer *server);
    //
    //        virtual std::string toString();
    //
    //        /** Return the agent's logger object
    //         *
    //         * @return A logger object.
    //         */
    //        Logger* getLogger();
    //
    //        /** Set whether to randomly stimulate atoms.
    //         *
    //         * @param newVal flag to generate noise or not.
    //         */
    //        void setNoiseFlag(bool newVal);
    //
    //        // The target lobe STI and LTI values are not only initial values, but
    //        // also values that are returned to if the values exit their
    //        // acceptable ranges
    //        long targetLobeSTI; //!< homeostatic centre for AtomSpace STI funds
    //        long targetLobeLTI; //!< homeostatic centre for AtomSpace STI funds
    //
    //        //! the interval to keep AtomSpace STI funds within
    //        long acceptableLobeSTIRange[2];
    //        //! the interval to keep AtomSpace LTI funds within
    //        long acceptableLobeLTIRange[2];
    //
    //        /** Set whether link atoms should be updated.
    //         *
    //         * @param flag flag to update links or not.
    //         */
    //        void setUpdateLinksFlag(bool f);
    //
    //        /** Get whether link atoms should be updated.
    //         *
    //         * @return Whether links are updated or not.
    //         */
    //        bool getUpdateLinksFlag() const;
    //
    //        inline AttentionValue::sti_t getSTIAtomWage() const
    //            { return STIAtomWage; }
    //        inline AttentionValue::lti_t getLTIAtomWage() const
    //            { return LTIAtomWage; }
    //
    //        inline void setRentType(rentType_t t)
    //            { rentType = t; }
    //        inline rentType_t getRentType() const
    //            { return rentType; }
    //
    //        inline void setAmnesty(AttentionValue::sti_t t)
    //            { amnesty = t; }
    //        inline AttentionValue::sti_t getAmnesty() const
    //            { return amnesty; }
    //
    //        inline void setRentFunctionParameters(std::vector<double> newParameters)
    //            { rentFunctionParams = newParameters; }
    //        inline std::vector<double> getRentFunctionParameters()
    //            { return rentFunctionParams; }
    //
    //    }; // class
}
