/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.genifer;

import jcog.opencog.atom.MapToAtom;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.OCMind;
import jcog.opencog.attention.LearnHebbian;
import jcog.opencog.attention.RandomStimulation;
import jcog.opencog.attention.SpreadImportance;
import jcog.opencog.swing.AttentionControlPanel;
import jcog.opencog.swing.GraphPanel;
import jcog.opencog.swing.GraphView;
import jcog.spacegraph.swing.SwingWindow;
import org.armedbear.lisp.LispObject;
import org.armedbear.lisp.StandardObject;

/**
 *
 * @author seh
 */
public class TestGeniferGraph {
    
    public static class AtomizeGenifer {

        MapToAtom<String> types;
        MapToAtom<String> symbols;
        
        private final OCMind mind;
        private final GeniferLisp lisp;
        
        public AtomizeGenifer(GeniferLisp gl, OCMind mind) {
            super();
            
            this.mind = mind;
            this.lisp = gl;
            
            types = new MapToAtom(AtomType.conceptNode, mind);
            symbols = new MapToAtom(AtomType.conceptNode, mind);
            
            LispObject[] s = gl.getSymbols();
            for (LispObject lo : s) {
                indexObject(lo);
            }
            
            //System.out.println(gl.getMemory().getRules());
            //System.out.println(gl.getMemory().getFacts());
        }
        
        public Atom indexObject(LispObject lo) {
            final StandardObject so = (StandardObject)lo;
            if (lo == null)
                return null;
            
            final String id = lo.writeToString();
            final String lispType = lo.typeOf().writeToString().toUpperCase();
            
            if (symbols.has(id))
                return null;               

            final Atom a = symbols.get(id);
            final Atom type = types.get(lispType);

            mind.addEdge(AtomType.intensionalInheritanceLink, a, type);

            
            if (lo.listp()) {
                Atom head = indexObject(lo.car());
                Atom tail = indexObject(lo.cdr());
                
                if ((tail != null) && (head!=null)) {
                    mind.addEdge(AtomType.extensionalInheritanceLink, a, head, tail);
                }
                else if ((tail == null) && (head!=null)) {
                    mind.addEdge(AtomType.extensionalInheritanceLink, a, head);
                }
            }
            
            return a;
        }
        
        
    }
    
    public static void main(String[] args) {
        GeniferLisp gl = new GeniferLisp(new SimpleMemory());

        OCMind mind = new OCMind();
        
        new AtomizeGenifer(gl, mind);
        
        mind.printAtoms();

        mind.addAgent(new LearnHebbian());        
        mind.addAgent(new SpreadImportance());
        //mind.addAgent(new DecaySTI(0.5, (short)1));
        //mind.addAgent(new Forget(0.5, 20000, 40000));
        
        //mind.addAgent(new AddRandomHebbianEdges(0.5, 64, 8, 4000, 5000));
        mind.addAgent(new RandomStimulation(0.5, (short)200, 3));
        //mind.addAgent(new MessageTokenizer(0.5));

        new AttentionControlPanel(mind, 0.75).newWindow();          
        new SwingWindow(new GraphPanel(new GraphView(mind)), 800, 800, true);

        mind.start(0.05);
        
//        //gl.execute("INIT-TEST-MEM", gl.getMemory());
//        
//        //gl.execute("SYSTEM-TEST", gl.getMemory());
//        
//        //gl.induce();
//
//        List<LispObject> objects = Arrays.asList(gl.getSymbols());
//        for (LispObject lo : objects) {
//            StandardObject so = (StandardObject)lo;
//            System.out.println(lo.getClass().getSimpleName() + ": " + lo.writeToString() + " " + lo.typeOf().writeToString());
//            String lispType = lo.typeOf().writeToString();
//            if (lispType.equals("RULE-ITEM")) {
//                
//            } else if (lispType.equals("FACT-ITEM")) {
//                //        (format t "**** [~a] fact: ~a ~%" (id item) (fact item))
//                //        (setf tv (tv item))
//                //        (format t "  TV:           ~a ~%" (car tv))
//                //        (format t "  confidence:   ~a ~%" (cdr tv))
//                //        (format t "  justifies:    ~a ~%" (justifies    item))
//                //        (format t "  justified-by: ~a ~%" (justified-by item)))                
//                
//                System.out.println("(id " + lo.writeToString() + ")");
//                System.out.println(so.getInstanceSlotValue(Lisp.intern("ID", gl.defaultPackage)).javaInstance());
//                System.out.println(so.getInstanceSlotValue(Lisp.intern("FACT", gl.defaultPackage)).javaInstance());
//                System.out.println(so.getInstanceSlotValue(Lisp.intern("TV", gl.defaultPackage)).car().javaInstance());
//                System.out.println(so.getInstanceSlotValue(Lisp.intern("TV", gl.defaultPackage)).cdr().javaInstance());
//                System.out.println(so.getInstanceSlotValue(Lisp.intern("JUSTIFIES", gl.defaultPackage)).cdr().javaInstance());
//                System.out.println(so.getInstanceSlotValue(Lisp.intern("JUSTIFIED-BY", gl.defaultPackage)).cdr().javaInstance());                                
//            }
//        }

    }
}
