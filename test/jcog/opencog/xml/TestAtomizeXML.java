/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.xml;

import jcog.opencog.util.AtomizeXML;
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
public class TestAtomizeXML {

    public static void main(String[] args) {
        OCMind m = new OCMind();
        new AtomizeXML("/tmp/x.xml", m);
        
        m.printAtoms();
    }
}
