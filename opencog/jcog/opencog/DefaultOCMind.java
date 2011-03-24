/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog;

import jcog.opencog.MemoryAtomSpace;
import jcog.opencog.AtomTypes;
import jcog.opencog.OCMind;

/**
 *
 * @author seh
 */
public class DefaultOCMind extends OCMind {

    public DefaultOCMind() {
        super(new MemoryAtomSpace());
                
        AtomTypes t = new AtomTypes(atomspace);
    }

}
