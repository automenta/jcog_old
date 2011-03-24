package jcog.nars.swing;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import jcog.nars.reason.NAR;


/** controls NAR execution.  mostly horizontal */
public class NARExecutionPanel extends JPanel {

	private static final Insets panelInsets = new Insets(5,5,5,5);

	public NARExecutionPanel(NAR nar) {
		super(new GridBagLayout());
		
		GridBagConstraints gc = new GridBagConstraints();
		
		JButton stopButton = new JButton("[ ]");
		stopButton.setToolTipText("Stop");
		gc.gridx = 0;
		gc.weightx = 0;
		gc.insets = panelInsets;
		add(stopButton, gc);

		JButton runButton = new JButton("-->");
		runButton.setToolTipText("Run");
		gc.gridx = 1;
		gc.weightx = 0;
		add(runButton, gc);

		JButton stepButton = new JButton("...");
		stepButton.setToolTipText("Step");
		gc.gridx = 2;
		gc.weightx = 0;
		add(stepButton, gc);

		JLabel clockLabel = new JLabel("0");
		gc.gridx = 3;
		gc.weightx = 0;
		add(clockLabel, gc);
		
	}
	
}
