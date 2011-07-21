/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.language;

import edu.stanford.nlp.ling.CoreAnnotations.PartOfSpeechAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.TokensAnnotation;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.util.CoreMap;
import java.util.List;
import java.util.Properties;

/**
 *
 * @author seh
 */
public class CoreNLP extends StanfordCoreNLP {
    
    public static Properties getProperties(boolean parse) {
            Properties props = new Properties();
            if (parse) {
                // creates a StanfordCoreNLP object, with POS tagging, lemmatization, NER, parsing, and coreference resolution 
                props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref");
            }
            else {
                props.put("annotators", "tokenize, ssplit, pos");
            }
            return props;        
    }
    
    public CoreNLP(final boolean parse) {        
        super(getProperties(parse));
    }
    
    public String getPartOfSpeech(String w) {
        Annotation document = new Annotation(w);
    
        // run all Annotators on this text
        annotate(document);
        
        List<CoreMap> sentences = document.get(SentencesAnnotation.class);
            
        for(CoreMap sentence: sentences) {
              // traversing the words in the current sentence
              // a CoreLabel is a CoreMap with additional token-specific methods
              for (CoreLabel token: sentence.get(TokensAnnotation.class)) {
                // this is the text of the token
                //String word = token.get(TextAnnotation.class);
                
                // this is the POS tag of the token
                String pos = token.get(PartOfSpeechAnnotation.class);
                return pos;
                
                // this is the NER label of the token
                //String ne = token.get(NamedEntityTagAnnotation.class);       
              }

        }
        
        return null;            
    }
    
}
