/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.genifer;


/**
  A Generic-Memory "Fact" item
 * This class if for storing facts in memory.  Its base class is Formula.
 * Additionally a fact has an ID and a TV.
 * @author SEH
 */
public class Fact extends Formula {
    public final Formula formula;

    public final Truth truth;
        
    /** a list of clauses that this fact justifies */
    //public  final Formula[] justifies;

    /** a list of clauses that justify this fact */
    //public  final Formula[] justifiedBy;

    // TODO timestamp / time-marker?
    // YKY:  currently I think there's no need for timestamps

    public Fact(Formula formula, double truth, double confidence) {
        super();
        this.formula = formula;
        this.truth = new SimpleTruth(truth, confidence);
        //this.justifies = justifies;         // Formula[]
        //this.justifiedBy = justifiedBy;     // Formula[]
    }
    
    public Fact(Formula formula, Truth t) {
        super();
        this.formula = formula;
        this.truth = t;
    }

    @Override
    public String toString() {
        return formula.toString() + "<" + truth + ">";
    }
    
    
}
