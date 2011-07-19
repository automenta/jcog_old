/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog;

import java.util.UUID;

/**
 *
 * @author seh
 */
public class OCType extends Atom {
    private final String name;
    
    public OCType(final String typeid) {
        super(new UUID((long)typeid.hashCode(), (long)typeid.hashCode()));
        this.name = typeid;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
    
    
}
