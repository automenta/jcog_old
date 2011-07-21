/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.genifer;

import org.armedbear.lisp.Cons;

/**
 * Wraps an ABCL "Cons" as a Formula
 * @author seh
 */
public class Sexp extends Formula {
    
    public final Cons cons;

    public Sexp(Cons cons) {
        super();
        this.cons = cons;
    }

    @Override
    public String toString() {
        return cons.writeToString();
    }
    
}
