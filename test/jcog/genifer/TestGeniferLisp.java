/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.genifer;

import java.util.Arrays;
import java.util.List;
import jcog.genifer.swing.GeniferBrowserPanel;
import jcog.spacegraph.swing.SwingWindow;
import junit.framework.TestCase;
import org.armedbear.lisp.*;

/**
 *
 * @author seh
 */
public class TestGeniferLisp extends TestCase {
    private static Object defaultPackage;

    public void testMemory() {
        GeniferLisp gl = new GeniferLisp(new SimpleMemory());

        GeniferBrowserPanel gPanel = new GeniferBrowserPanel(gl);
        new SwingWindow(gPanel, 800, 600, true);
        
        gl.execute("INIT-TEST-MEM", gl.getMemory());
        
        //gl.execute("SYSTEM-TEST", gl.getMemory());
        
        //gl.induce();

        List<LispObject> objects = Arrays.asList(gl.getSymbols());
        for (LispObject lo : objects) {
            StandardObject so = (StandardObject)lo;
            System.out.println(lo.getClass() + ": " + lo.writeToString() + " " + lo.typeOf().writeToString());
            String lispType = lo.typeOf().writeToString();
            if (lispType.equals("RULE-ITEM")) {
                
            } else if (lispType.equals("FACT-ITEM")) {
                //        (format t "**** [~a] fact: ~a ~%" (id item) (fact item))
                //        (setf tv (tv item))
                //        (format t "  TV:           ~a ~%" (car tv))
                //        (format t "  confidence:   ~a ~%" (cdr tv))
                //        (format t "  justifies:    ~a ~%" (justifies    item))
                //        (format t "  justified-by: ~a ~%" (justified-by item)))                
                
                System.out.println("(id " + lo.writeToString() + ")");
                System.out.println(so.getInstanceSlotValue(Lisp.intern("ID", gl.defaultPackage)).javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("FACT", gl.defaultPackage)).javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("TV", gl.defaultPackage)).car().javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("TV", gl.defaultPackage)).cdr().javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("JUSTIFIES", gl.defaultPackage)).cdr().javaInstance());
                System.out.println(so.getInstanceSlotValue(Lisp.intern("JUSTIFIED-BY", gl.defaultPackage)).cdr().javaInstance());                                
            }
        }
        
        gPanel.refresh();
    }

    public static void main(String[] args) {
        new TestGeniferLisp().testMemory();
     }
}
