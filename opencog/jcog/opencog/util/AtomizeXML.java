/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.util;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.OCMind;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 *
 * @author seh
 */
public class AtomizeXML extends DefaultHandler {
    //TODO double-check the ordering of some of the edges created here
    LinkedList<Atom> path = new LinkedList();
    private final OCMind mind;
    private final String uri;
    private final Map<String, Atom> predicates = new HashMap();

    public AtomizeXML(String uri, OCMind mind) {
        this.mind = mind;
        this.uri = uri;
        final Atom header = mind.addVertex(AtomType.conceptNode, this.getClass().getName() + "(" + uri + ")");
        path.addLast(header);
        try {
            SAXParserFactory factory = SAXParserFactory.newInstance();
            SAXParser saxParser = factory.newSAXParser();
            saxParser.parse(uri, this);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Atom getPredicate(String qName) {
        Atom a = predicates.get(qName.toLowerCase());
        if (a == null) {
            a = mind.addVertex(AtomType.conceptNode, uri + "#" + qName);
            predicates.put(qName, a);
        }
        return a;
    }

    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        final Atom tagPredicateAtom = getPredicate(qName);
        final Atom parent = path.getLast();
        
        final Atom e = mind.addVertex(AtomType.conceptNode, qName);
        
        mind.addEdge(AtomType.extensionalInheritanceLink, parent, e);
        mind.addEdge(AtomType.intensionalInheritanceLink, tagPredicateAtom, e);
        
        
        for (int i = 0; i < attributes.getLength(); i++) {
            String name = attributes.getQName(i);
            Atom predicateAtom = getPredicate(qName + "." + name);
            String value = attributes.getValue(i);
            Atom valueAtom = mind.addVertex(AtomType.conceptNode, value);
            Atom edge = mind.addEdge(AtomType.evaluationLink, name, e, valueAtom);
            mind.addEdge(AtomType.intensionalInheritanceLink, predicateAtom, edge);
        }
        
        path.addLast(e);
    }

    public void endElement(String uri, String localName, String qName) throws SAXException {
        path.pop();
    }

    public void characters(char[] ch, int start, int length) throws SAXException {
        final String s = new String(ch, start, length);
        final Atom parent = path.getLast();
        final Atom valueAtom = mind.addVertex(AtomType.conceptNode, s);
        mind.addEdge(AtomType.extensionalInheritanceLink, parent, valueAtom);
    }
    
}
