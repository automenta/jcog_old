/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.nars;

import java.util.logging.Level;
import java.util.logging.Logger;
import jcog.nars.reason.DefaultNAR;
import jcog.nars.reason.NAR;
import junit.framework.TestCase;

/**
 *
 * @author seh
 */
public class TestParse extends TestCase {
    
    public void testParse() {
        
        DefaultNAR n = new DefaultNAR();
        n.getMemory().getLogger().setLevel(Level.OFF);
        
        n.getExperience().eval("<robin --> bird>. %1.00;0.90% {0: 1}");
        n.cycle(10);
        n.getExperience().eval("<penguin --> bird>. %0.80;0.90% {10: 2}");
        n.cycle(10);
        n.getExperience().eval("<swan --> bird>. %1.00;0.80% {20: 3}");
        n.cycle(10);
        n.getExperience().eval("<?1 --> bird>?  {30: 4}");
        n.cycle(150);
    }

    public void testParse2() {
        DefaultNAR n = new DefaultNAR();
        n.getMemory().getLogger().setLevel(Level.OFF);
        
        n.getExperience().eval("<coffee --> beverage>.");
        n.getExperience().eval("<Java --> coffee>.");
        n.getExperience().eval("(--,<Java --> coffee>).");
        n.cycle(10);
        n.getExperience().eval("<Java --> coffee>?");
        n.cycle(10);
        n.getExperience().eval("<tea --> beverage>?");
        n.cycle(10);
        n.getExperience().eval("<coffee --> beverage>?");
        n.cycle(10);

    }
    
}
