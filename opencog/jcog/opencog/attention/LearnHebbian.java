package jcog.opencog.attention;

import java.util.ArrayList;
import java.util.List;
import jcog.opencog.Atom;
import jcog.opencog.AtomTypes;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;

/** Agent that carries out simple Hebbian learning.
*
* @see http://en.wikipedia.org/wiki/Hebbian_learning
* @note Only updates existing HebbianLinks. Still need a HebbianMiningAgent...
* @todo Support SymmetricInverseHebbianLinks and AsymmetricHebbianLinks
* @todo Support Hebbian links with arity > 2
*/
public class LearnHebbian extends MindAgent {

    float tcDecayRate = 0.1f;
    private OCMind mind;

    public LearnHebbian() {
        super();
    }
    
    
    @Override
    public void run(OCMind mind) {
        
        this.mind = mind;
        
        for (Atom e : mind.getAtoms(AtomTypes.SymmetricHebbianLink, false)) {
            //This is a HACK that only works on SymmetricHebbianLinks of arity=2

            if (mind.getArity(e)!=2)
                continue;
            // for each hebbian link, find targets, work out conjunction and convert
            // that into truthvalue change. the change should be based on existing TV.
            
            List<Atom> members = new ArrayList(mind.getIncidentVertices(e));            
            double newTC = getTargetConjunction(members.get(0), members.get(1));
            
            // old link strength decays
            double oldTC = mind.getTruth(e).getMean();
            
            //a->getTV(h)->getMean();
            //if (new_tc != old_tc) isDifferent = true;

            // otherwise just update link weights
            double tc = (tcDecayRate * newTC) + ( (1.0f - tcDecayRate) * oldTC);            
            if (tc < 0.0f) tc = 0.0f;
            
            mind.getTruth(e).setMean(tc);
        }

    }

    public double getTargetConjunction(Atom a, Atom b) {
        double aNSTI = mind.getNormalizedSTI(a);
        double bNSTI = mind.getNormalizedSTI(a);
        double tc = aNSTI * bNSTI;
        return tc;
    }
    
//	private:
//	    AtomSpace* a;
//
//		/** Work out the conjunction between a series of handles.
//		 *
//		 * The returned value is the correlation between distance in/out of
//		 * the attentional focus. STI of atoms are normalised (separately
//		 * for atoms within the attentional focus and those below it) and then
//		 * multiplied. Only returns a non zero value if at least one atom
//		 * is in the attentional focus.
//		 *
//		 * @param handles A vector of handles to calculate the conjunction for.
//		 * @return conjunction between -1 and 1.
//		 * @todo create a method for working out conjunction between more than
//		 * two atoms.
//		 */
//	    float targetConjunction(std::vector<Handle> handles);
//
//		/** Transform STI into a normalised STI value between -1 and 1.
//		 *
//		 * @param s STI to normalise.
//		 * @return the normalised STI between -1.0 and 1.0
//		 */
//	    float getNormSTI(AttentionValue::sti_t s);
//
//		/** Rearrange the the vector so that the one with a positive normalised
//		 * STI is at the front.
//		 *
//		 * @param outgoing Vector to rearrange.
//		 * @return rearranged vector.
//		 */
//	    std::vector<Handle>& moveSourceToFront(std::vector<Handle> &outgoing);
//
//	    /** Set the agent's logger object
//	     *
//	     * Note, this will be deleted when this agent is.
//	     *
//	     * @param l The logger to associate with the agent.
//	     */
//	    void setLogger(Logger* l);
//
//	    Logger *log; //!< Logger object for Agent
//
//	public:
//
//	    virtual const ClassInfo& classinfo() const { return info(); }
//	    static const ClassInfo& info() {
//	        static const ClassInfo _ci("opencog::HebbianUpdatingAgent");
//	        return _ci;
//	    }
//
//	    HebbianUpdatingAgent();
//	    virtual ~HebbianUpdatingAgent();
//	    virtual void run(CogServer *server);
//
//	    /** Return the agent's logger object
//	     *
//	     * @return A logger object.
//	     */
//	    Logger* getLogger();
//
//	    //! Whether to convert links to/from InverseHebbianLinks as necessary.
//	    bool convertLinks;
//
//	    //! Maximum allowable LTI of a link to be converted.
//	    AttentionValue::lti_t conversionThreshold;
//
//		/** Update the TruthValues of the HebbianLinks in the AtomSpace.
//	     *
//	     * @todo Make link strength decay parameter configurable. Currently
//	     * hard-code, ugh.
//	     */
//	    void hebbianUpdatingUpdate();
//
//	}; // class




}
