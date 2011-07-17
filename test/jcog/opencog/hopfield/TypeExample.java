/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.hopfield;

import jcog.opencog.DefaultOCMind;
import jcog.opencog.OCMind;
import jcog.opencog.swing.GraphView;

/**
 *
 * @author seh
 */
public class TypeExample {
    
    public static void main(String[] args) {
        OCMind mind = new DefaultOCMind();
        
        GraphView.newGraphWindow(mind);        


    }
    
}
