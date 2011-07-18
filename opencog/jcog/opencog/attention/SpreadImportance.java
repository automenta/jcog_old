package jcog.opencog.attention;

import java.util.Collection;
import java.util.List;
import jcog.opencog.Atom;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;

/** Spreads short term importance along HebbianLinks.
 *
 * Currently only spreads along Symmetric and Inverse HebbianLinks.
 *
 * @todo Spread along asymmetric hebbian links too.
 * @todo Spread along symmetric inverse hebbian links too.
 * @todo Optionally spread long term importance.
 */
public class SpreadImportance extends MindAgent {

    short spreadThreshold;
    short stealingLimit;
    float importanceSpreadingMultiplier;
    //RecentLong amountSpread;
    double stiTransferRate = 10.0;
    int totalSurplus = 0;
    
    @Override
    public void run(OCMind mind) {
        for (Atom a : mind.getAtoms()) {
            final int sti = mind.getSTI(a);
            
            int avgSurroundingSTI = 0;
            int count = 0;
            
            final Collection<Atom> ie = mind.getIncidentEdges(a);
            if (ie!=null) {
                for (Atom e : ie) {
                    avgSurroundingSTI += mind.getSTI(e);
                    count++;
                }
            }
            
            final Collection<Atom> iv = mind.getIncidentVertices(a);
            if (iv!=null){
                for (Atom e : iv) {
                    avgSurroundingSTI += mind.getSTI(e);
                    count++;
                }
            }
            
            if (count!=0) {
                avgSurroundingSTI /= count;
            }
            else {
                continue;
            }
            
            final int diffusion_momentum = 60;
            
            short difference = (short)(sti - avgSurroundingSTI);
            if (avgSurroundingSTI < sti) {
                difference /= diffusion_momentum;
                if (difference < 1) difference = 1;
                
                addStimulus(a, (short)-difference);
                totalSurplus+=difference;
            }
            else if (avgSurroundingSTI > sti) {
                difference = (short)(avgSurroundingSTI - sti);
                difference /= diffusion_momentum;
                if (difference < 1) difference = 1;

                difference = (short)Math.min(difference, totalSurplus);
                
                if (totalSurplus >= difference) {
                    addStimulus(a, (short)difference);
                    totalSurplus-=difference;
                }                    
            }
        }
    }
    
//    @Override
//    public void run(OCMind mind) {
//
//        double totalDistributedPlus = 0, totalDistributedMinus = 0;
//        
//        //TODO this is a hack to just get some spreading activity that only works for SymmetricHebbianLinks of arity=2
//        //for (Atom e : mind.getAtoms(AtomTypes.SymmetricHebbianLink, false)) {
//        for (Atom e : mind.getEdges()) {
//            Collection<Atom> linked = mind.getIncidentVertices(e);
//            if (linked.size() == 0)
//                continue;
//                        
////            int parentSTI = mind.getSTI(e);
////            for (Atom a : linked) {
////                if (parentSTI > mind.getSTI(a)+linked.size()) {
////                    addStimulus(a, (short)1);
////                    totalDistributedPlus += 1.0;
////                }
////                else if (parentSTI < mind.getSTI(a)-linked.size()) {
////                    addStimulus(a, (short)-1);
////                    totalDistributedMinus += 1.0;
////                }
////            }
//            
//            
////            int averageSTI = mind.getSTI(e);
////            for (Atom a : linked) {
////                averageSTI += mind.getSTI(a);
////            }
////            averageSTI /= (1 + linked.size());
////            
////            for (Atom a : linked) {
////                if (averageSTI > mind.getSTI(a)) {
////                    addStimulus(a, (short)1);
////                    totalDistributedPlus += 1.0;
////                }
////                else if (averageSTI < mind.getSTI(a)) {
////                    addStimulus(a, (short)-1);
////                    totalDistributedMinus += 1.0;
////                }
////            }
//            
////            if (linked.size() == 2) {
////                Atom a = linked.get(0);
////                Atom b = linked.get(1);
////
////                short aSTI = mind.getSTI(a);
////                short bSTI = mind.getSTI(b);
////                short absDifference = (short) Math.abs(aSTI - bSTI);
////
////                if (absDifference == 0) continue;
////                
////                //TODO this rate calculation may not be correct
////
////                short rate = (short) (mind.getTruth(e).getMean() * stiTransferRate);
////
////                if (rate > 0) {
////                    if (rate > absDifference) {
////                        rate = absDifference;
////                    }
////
////                    rate = (short)Math.min(Math.abs(aSTI-bSTI), rate);
////                            
////                    if (aSTI > bSTI) {
////                        mind.getAttention(a).addSTI((short) (-rate));
////                        mind.getAttention(b).addSTI((short)(rate));
////                        totalDistributed += Math.abs(rate);
////                    } else if (bSTI > aSTI) {
////                        mind.getAttention(b).addSTI((short) (-rate));
////                        mind.getAttention(a).addSTI((short)(rate));
////                        totalDistributed += Math.abs(rate);
////                    }
////                }
////            }
//        }
//        
//        //System.out.println("totalDistributed=" + totalDistributedPlus + " -" + totalDistributedMinus);
//
//    }

    public SpreadImportance() {
        super();
    }

    /** Spread importance for an atom.
     *
     * @param h The handle for the atom to spread importance for.
     */
    protected void spreadAtomImportance(Atom a) {
    }

    /** Spread importance along Hebbian links. */
    protected void spreadImportance() {
    }

    //! Sum total difference for an atom
    protected int sumTotalDifference(Atom source, List<Atom> links) {
        //TODO
        return 0;
    }

    //! Sum difference for one link
    protected int sumDifference(Atom source, Atom link) {
        //TODO
        return 0;
    }

    //! Calculate the difference for an inverse link
    protected float calcInverseDifference(short s, short t, float weight) {
        //TODO
        return 0;
    }

    //! Calculate the difference for a normal Hebbian link
    protected float calcDifference(short s, short t, float weight) {
        //TODO
        return 0;
    }

    /** Set minimal amount of STI necessary for an atom to have before it
     * spreads STI.
     *
     * @param t the threshold.
     */
    void setSpreadThreshold(short t) {
        spreadThreshold = t;
    }

    /** Set the multiplier for converting the HebbianLink TruthValue to STI.
     *
     * If multiplier is zero, then \b all STI above the threshold is evenly
     * distributed.
     *
     * @param m the multiplier.
     */
    void setImportanceSpreadingMultiplier(float m) {
        importanceSpreadingMultiplier = m;
    }
}
