package jcog.nars.swing;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JTextArea;
import jcog.nars.reason.NAR;


/** 
 * Inference control: The administrator can run the inference engine for a predetermined number of steps, run it continuously, or stop its running. 
 * @see http://code.google.com/p/open-nars/wiki/GUIRequest
 * @see http://code.google.com/p/open-nars/wiki/GUIGuide
 */
public class InferencePanel extends JPanel {

	private JTextArea output;

	public InferencePanel(NAR nar) {
		super(new BorderLayout());
		
		output = new JTextArea();
		output.setEditable(false);
		add(output, BorderLayout.CENTER);
		
		add(new NARExecutionPanel(nar), BorderLayout.SOUTH);
	}
	
}
