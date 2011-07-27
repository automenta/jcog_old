package jcog.opencog.attention;

import com.google.common.base.Predicate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import jcog.opencog.Atom;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;
import org.apache.commons.collections15.IteratorUtils;

/** The ForgettingAgent, carries out the forgetting process in OpenCog Prime. 
 * 
 * It does based on the LTI of Atoms. Low LTI indicates that an atom has not been
 * of any use for a long time, and additionally, isn't near any other important
 * atoms. The latter condition is because the ImportanceSpreadingAgent would
 * otherwise increase the STI of the atom, by moving STI from nearby important
 * atoms, and increase the likelihood of the atom in question being:
 *
 * 1. used in mind processes, and thus
 * 2. rewarded with stimulus which later gets exchanged for STI. 
 *
 * The ForgettingAgent also takes into account the VLTI of an atom. This is a
 * boolean value that indicates whether the atom in fact is allowed to be
 * forgotten. This allows a mechanism for ensuring that highly important
 * information will never be forgotten, even if it's used only very very rarely.
 *
 * Forgetting can be tuned via two parameters:
 *
 * 1. the maximum LTI value that can be forgotten, and
 * 2. the percentage of the AtomSpace to forget (typically this would be very low!) 
 *
 * These work in concert to limit how much and what atoms are forgotten. If only
 * one parameter is set, then the other has free reign. I.e. a certain percentage
 * of the AtomSpace will always be forgotten regardless of their LTI, or, any atom
 * that drops below the maximum forgetting LTI will be forgotten. 
 */
public class Forget extends MindAgent {
    
    int maxVerticesBeforePrune;
    int maxEdgesBeforePrune;
    
    
    /**
     * @param period
     * @param maxVerticesBeforePrune
     * @param maxEdgesBeforePrune 
     */
    public Forget(double period, int maxVerticesBeforePrune, int maxEdgesBeforePrune) {
        super();
        
        this.maxVerticesBeforePrune = maxVerticesBeforePrune;
        this.maxEdgesBeforePrune = maxEdgesBeforePrune;
        
        setPeriod(period);
                
    }
    
    @Override
    public void run(OCMind mind) {

        final Iterator<Atom> ia = mind.iterateAtomsByIncreasingSTI();
        
        int vertDifference = 0, edgeDifference = 0;

        int vc = mind.getVertexCount();
        int ec = mind.getEdgeCount();
        
        if (vc > maxVerticesBeforePrune) {
            vertDifference = vc - maxVerticesBeforePrune;
        }
        if (ec > maxEdgesBeforePrune) {
            edgeDifference = ec - maxEdgesBeforePrune;
        }        
        
        //int tested = 0;
        while (ia.hasNext() && ((vertDifference > 0) || (edgeDifference > 0)) ) {
            Atom a = ia.next();
            if (mind.isVertex(a)) {
                if (vertDifference > 0) {
                    removeVertex(a);                
                    vertDifference--;                
                }
            }
            else {
                if (edgeDifference > 0) {
                    removeEdge(a);
                    edgeDifference--;
                }
            }
            //tested++;
        }
        //System.out.println("forget iterated thru " + tested + " atoms; " + vertDifference + " " + edgeDifference);
        
        
    }
        
//
//        //TODO this is a hack that simply decays all atoms STIs toward 0        
//        short stiDecayRate = 1;
//        for (MindAgent agent : mind.getAgents()) {            
//            for (Atom at : agent.getStimulated()) {
//                int sti = mind.getSTI(at);
//                sti -= stiDecayRate;
//                sti = Math.max(Short.MIN_VALUE, sti);
//                mind.getAttention(at).setSTI((short)sti);                
//            }
//            
//        }

    }
    
    
    

//	private:
//	    AtomSpace* a;
//	    Logger *log; //!< Logger object for Agent
//
//	    /** Set the agent's logger object
//	     *
//	     * Note, this will be deleted when this agent is.
//	     *
//	     * @param l The logger to associate with the agent.
//	     */
//	    void setLogger(Logger* l);
//
//	public:
//
//	    virtual const ClassInfo& classinfo() const { return info(); }
//	    static const ClassInfo& info() {
//	        static const ClassInfo _ci("opencog::ForgettingAgent");
//	        return _ci;
//	    }
//
//	    //! Maximum LTI of an atom that can be forgot.
//	    AttentionValue::lti_t forgetThreshold;
//	    //! Percentage of AtomSpace to forget.
//	    float forgetPercentage;
//
//	    ForgettingAgent();
//	    virtual ~ForgettingAgent();
//	    virtual void run(CogServer *server);
//
//	    void forget(float p);
//
//	    /** Return the agent's logger object
//	     *
//	     * @return A logger object.
//	     */
//	    Logger* getLogger();
//
//	}; // class
//
//	/**
//	 * Comparison operator for using qsort on a list of Handles.
//	 * Returns them with ascending LTI and if equal in LTI,
//	 * then sorted ascending by TruthValue.
//	 */
//	struct ForgettingLTIThenTVAscendingSort {
//	    bool operator()(const Handle& h1, const Handle& h2) {
//	        AttentionValue::lti_t lti1, lti2;
//	        float tv1, tv2;
//
//	        lti1 = TLB::getAtom(h1)->getAttentionValue().getLTI();
//	        lti2 = TLB::getAtom(h2)->getAttentionValue().getLTI();
//
//	        tv1 = fabs(TLB::getAtom(h1)->getTruthValue().getMean());
//	        tv2 = fabs(TLB::getAtom(h2)->getTruthValue().getMean());
//
//	        if (lti1 != lti2) return lti1 < lti2;
//
//	        else return tv1 < tv2;
//	    }
//
//	};


