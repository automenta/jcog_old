package jcog.genifer;


/**
   structure for a rule / fact
 * @author SEH
 *
 * YKY:  This is used to send a result of fetch() back to the caller
 */
public class MemItem extends Formula {

    //  YKY:  This class should have 4 elements:  id, formula, tv
    public final Formula formula;
    public final Truth truth;

    public MemItem(Formula formula, double truth, double confidence) {
        super();
        this.formula = formula;
        this.truth = new SimpleTruth(truth, confidence);
    }

}
