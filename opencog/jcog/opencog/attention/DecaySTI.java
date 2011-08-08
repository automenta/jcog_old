package jcog.opencog.attention;

import java.util.Iterator;
import jcog.opencog.Atom;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;

public class DecaySTI extends MindAgent {

    private short decayRate;
    
    /**
     * 
     * @param period
     * @param decayRate (specified as a positive number)
     */
    public DecaySTI(double period, short decayRate) {
        super(period);
        setDecayRate(decayRate);
    }

    
    @Override
    protected void run(OCMind mind) {
        Iterator<Atom> ia = mind.iterateAtoms();
        while (ia.hasNext()) {
            Atom a = ia.next();
            if (mind.getSTI(a) > Short.MIN_VALUE)
                addStimulus(a, (short)-decayRate);
        }
    }
    

//    virtual const ClassInfo& classinfo() const { return info(); }
//    static const ClassInfo& info() {
//        static const ClassInfo _ci("opencog::STIDecayingAgent");
//        return _ci;
//    }
//
//    STIDecayingAgent();
//    virtual ~STIDecayingAgent();
//    virtual void run(CogServer *server);
//	void STIDecayingAgent::run(CogServer *cogserver)
//	{
//	    logger().info("[STIDecayingAgent] run");
//	    cogserver->getAtomSpace()->decayShortTermImportance();;
//	}

    public void setDecayRate(short decayRate) {
        this.decayRate = decayRate;
    }


    
	
}
