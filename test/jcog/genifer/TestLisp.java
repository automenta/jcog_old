/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.genifer;

import java.util.Arrays;
import java.util.List;
import org.armedbear.lisp.Cons;
import org.armedbear.lisp.Interpreter;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.Package;
import org.armedbear.lisp.Packages;

/**
 *
 * @author SEH
 */
public class TestLisp {

    public static void main(String[] args) {
        Interpreter lisp = Interpreter.createInstance();
        lisp.eval("(format t \"Hello, world!\")");

        Package defaultPackage = Packages.findPackage("CL-USER");
        
        lisp.eval("(load \"lisp/main.lisp\")");

        List<LispObject> symbols = Arrays.asList(((Cons)defaultPackage.getSymbols().javaInstance()).copyToArray());
        for (LispObject lp : symbols)
            System.out.println(lp);

    }
}
