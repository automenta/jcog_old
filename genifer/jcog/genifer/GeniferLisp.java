/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.genifer;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import org.armedbear.lisp.*;
import org.armedbear.lisp.Package;

/**
 *
 * @author SEH
 */
public class GeniferLisp implements Genifer {

    public final Package defaultPackage;
    private final LispObject lispMemory;
    private final Interpreter lisp;
    public final Memory memory;

    public GeniferLisp(Memory memory) {
        super();

        this.memory = memory;

        lisp = Interpreter.createInstance();

        lisp.eval("(load \"genifer/lisp/main.lisp\")");
        //lisp.eval("(load \"/app/genifer/genifer/lisp/main.lisp\")");
        
        lispMemory = lisp.eval("*generic-memory*");

        //System.out.println("packages: " + Arrays.asList(Packages.getAllPackages()));

        defaultPackage = Packages.findPackage("CL-USER");

    }

    public Memory getMemory() {
        return memory;
    }

    public LispObject eval(String e) {
        return lisp.eval(e);
    }

    public void abduce(String query) {
    }

    public void induce() {
        System.out.println("induce()");
        lisp.eval("(induce)");
    }

    public void backwardChain(String query) {
    }

    public void setDebug(int level) {
    }

//    public void testSystem() {
//        lisp.eval("(system-test)");
//    }
    public String toString() {
        return "[dump memory]";
    }

    protected void update() {
    }

    public LispObject[] getSymbols() {
        //System.out.println( defaultPackage.getAccessibleSymbols() );// findAccessibleSymbol("*generic-memory*");
        //System.out.println(genMem);

//        Object j = defaultPackage.getSymbols().
//        System.out.println(((Cons)defaultPackage.getSymbols().javaInstance()));
        return lispMemory.copyToArray();
        //return ((Cons)defaultPackage.getSymbols().javaInstance()).copyToArray();        
    }

    public void execute(String function, Object... params) {
        try {
            Symbol voidsym =
                defaultPackage.findAccessibleSymbol(function);
            Function voidFunction = (Function) voidsym.getSymbolFunction();
            
            LispObject[] loArgs = new LispObject[params.length];
            int i = 0;
            for (Object o : params) {
                loArgs[i++] = new JavaObject(o);
            }            
                        
            voidFunction.execute(loArgs);
            
        } catch (Throwable t) {
            System.err.println(t);
            t.printStackTrace();
        }
    }
//    public static void main(String[] args) {
//        new SwingWindow(new GeniferPanel(new GeniferLisp(new RAMMemory())), 800, 600, true);
//    }
}