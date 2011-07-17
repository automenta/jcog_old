/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.nlp;

import edu.stanford.nlp.ling.CoreAnnotations.CollapsedCCProcessedDependenciesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.CorefGraphAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.NamedEntityTagAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.PartOfSpeechAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TextAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TreeAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.IndexedWord;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.semgraph.SemanticGraph;
import edu.stanford.nlp.trees.semgraph.SemanticGraphEdge;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.IntTuple;
import edu.stanford.nlp.util.Pair;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import jcog.opencog.Atom;
import jcog.opencog.AtomTypes;
import jcog.opencog.DefaultOCMind;
import jcog.opencog.OCMind;
import jcog.opencog.swing.GraphView;
import jcog.spacegraph.swing.SwingWindow;

/**
 *
 * @author seh
 */
public class TestCoreNLP extends JPanel {
    private StanfordCoreNLP pipeline;
    
    public TestCoreNLP() {
        super(new BorderLayout());
    
        initNLP();
        
        final JTextArea text = new JTextArea("");
        text.setWrapStyleWord(true);
        text.setLineWrap(true);
        text.setText("Natural language processing is a field of computer science and linguistics concerned with the interactions between computers and human languages.");
        text.selectAll();

        add(new JScrollPane(text), BorderLayout.CENTER);
        
        JButton b = new JButton("Graph");
        b.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        graph(text.getText());
                    }                    
                });
            }            
        });
        add(b, BorderLayout.SOUTH);
    }

    protected void initNLP() {
        //http://nlp.stanford.edu/software/corenlp.shtml#Usage
        // creates a StanfordCoreNLP object, with POS tagging, lemmatization, NER, parsing, and coreference resolution 
        Properties props = new Properties();
        //props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref");
        props.put("annotators", "tokenize, ssplit, pos, parse");

        pipeline = new StanfordCoreNLP(props);
        
    }
    
    public void graph(final String text) {
        OCMind mind = new DefaultOCMind();
        
        String outputText = "";
        
        // create an empty Annotation just with the given text
        Annotation document = new Annotation(text);

        // run all Annotators on this text
        pipeline.annotate(document);

        // these are all the sentences in this document
        // a CoreMap is essentially a Map that uses class objects as keys and has values with custom types
        List<CoreMap> sentences = document.get(SentencesAnnotation.class);

        for (CoreMap sentence : sentences) {
            // traversing the words in the current sentence
            // a CoreLabel is a CoreMap with additional token-specific methods
            
            List<CoreLabel> tokens = sentence.get(TokensAnnotation.class);
            for (CoreLabel token : tokens) {                
                // this is the text of the token
                String word = token.get(TextAnnotation.class);
                // this is the POS tag of the token
                String pos = token.get(PartOfSpeechAnnotation.class);
                // this is the NER label of the token
                String ne = token.get(NamedEntityTagAnnotation.class);
                
                //System.out.println(word + " " + pos + " " + ne);
            }

            // this is the parse tree of the current sentence
            Tree tree = sentence.get(TreeAnnotation.class);
            outputText += tree.toString();

            // this is the Stanford dependency graph of the current sentence
            SemanticGraph dependencies = sentence.get(CollapsedCCProcessedDependenciesAnnotation.class);
            outputText += dependencies.toString();
            
            understand(mind, tokens, dependencies);
        }

        // this is the coreference link graph
        // each link stores an arc in the graph; the first element in the Pair is the source, the second is the target
        // each node is stored as <sentence id, token id>. Both offsets start at 1!
        List<Pair<IntTuple, IntTuple>> graph = document.get(CorefGraphAnnotation.class);
        if (graph!=null)
            outputText += graph.toString();        
        
        
        JPanel w = new JPanel(new BorderLayout());
        w.add(GraphView.newGraphPanel(mind), BorderLayout.CENTER);
        //w.add(new JScrollPane(new JTextArea(outputText)), BorderLayout.SOUTH);        
        new SwingWindow(w, 800, 600, false);
    }
    
    public static void main(String[] args) {
        new SwingWindow(new TestCoreNLP(), 700, 300, true);
    }

    public static void understand(OCMind mind, List<CoreLabel> tokens, SemanticGraph semantics) {
        Map<CoreLabel, Atom> wordAtoms = new HashMap();        
        Atom[] wordSequence = new Atom[tokens.size()];
        
        for (IndexedWord w : semantics.vertexList()) {
            if (!wordAtoms.containsKey(w)) {
                Atom a = mind.addVertex(AtomTypes.ConceptNode, w.toString());
                wordAtoms.put(w, a);
                wordSequence[w.index()-1] = a;
            }                
                        
        }
        
        int i = 0;        
        for (CoreLabel cl : tokens) {            
            //Adds words not contained as semantic vertices. these are usually prepositions like 'a', 'and', 'of', etc
            if (wordSequence[i] == null) {
                String label = cl.getString(TextAnnotation.class);
                Atom a = mind.addVertex(AtomTypes.ConceptNode, label + "." + Atom.newIDString());
                wordAtoms.put(cl, a);
                wordSequence[i] = a;
            }
            i++;
        }
        
        Atom seq = mind.addEdge(AtomTypes.OrderedLink, wordSequence);

        Map<String, Atom> gramRels = new HashMap();
        
        for (SemanticGraphEdge e : semantics.edgeList()) {
            Atom s = wordAtoms.get(e.getSource());
            Atom t = wordAtoms.get(e.getTarget());
            if (s == null) {
                System.out.println("has null source: " + e);
                continue;
            }
            else if (t == null) {
                System.out.println("has null target: " + e);
                continue;                
            }
            
            Atom ee = mind.addEdge(AtomTypes.INTENSIONAL_INHERITANCE_LINK, s, t);
        
            String rs = e.getRelation().getShortName();
            
            Atom a = gramRels.get(rs);
            if (a==null) {
                a = mind.addVertex(AtomTypes.ConceptNode, rs);
                gramRels.put(rs, a);
            }
            
            Atom er = mind.addEdge(AtomTypes.INTENSIONAL_INHERITANCE_LINK, a, ee);
        }
    }
}
