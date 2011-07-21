/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.genifer;

import java.util.LinkedList;
import java.util.List;

/**
  This class is for storing rules in memory.
  Each rule has a Formula plus the following info:
     id
     w            = size of support (ie, total number of times the rule is involved in proofs)
     e+           = positive examples
     e-           = negative examples
     parents      = a list of ancestor rules of this rule
     children     = a list of rules that this rule is ancestor to
 * @author SEH, YKY
*/
public class Rule {
    public final Formula formula;
    
    private double w;
    
    public final List<Formula> ePositive = new LinkedList();
    public final List<Formula> eNegative = new LinkedList();
    
    public final List<Rule> parents = new LinkedList();
    public final List<Rule> children = new LinkedList();

    public Rule(Formula formula, double w) {
        this.w = w;
        this.formula = formula;
    }

    public double getW() {
        return w;
    }

    public void setW(double w) {
        this.w = w;
    }        

    @Override
    public String toString() {
        return formula.toString() + "<" + w + ">";
    }
    
}
