/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import sun.org.mozilla.javascript.internal.Context;
import sun.org.mozilla.javascript.internal.ScriptableObject;

/**
 *
 * @author seh
 */
public class ConsoleWindow extends JFrame {
    PipedInputStream piOut;
    PipedInputStream piErr;
    PipedOutputStream poOut;
    PipedOutputStream poErr;
    protected JTextArea textArea = new JTextArea();

    public ConsoleWindow()  {
        super();
        
//        // Set up System.out
//        piOut = new PipedInputStream();
//        poOut = new PipedOutputStream(piOut);
//        System.setOut(new PrintStream(poOut, true));
//        
//
//        // Set up System.err
//        piErr = new PipedInputStream();
//        poErr = new PipedOutputStream(piErr);
//        System.setErr(new PrintStream(poErr, true));

        // Add a scrolling text area
        textArea.setEditable(false);
        textArea.setRows(20);
        textArea.setColumns(50);
        getContentPane().add(new JScrollPane(textArea), BorderLayout.CENTER);
        pack();
        setVisible(true);

//        // Create reader threads
//        new ReaderThread(piOut).start();
//        new ReaderThread(piErr).start();
    }

    class ReaderThread extends Thread {
        PipedInputStream pi;

        ReaderThread(PipedInputStream pi) {
            this.pi = pi;
        }

        public void run() {
            final byte[] buf = new byte[1024];
            try {
                while (true) {
                    final int len = pi.read(buf);
                    if (len == -1) {
                        break;
                    }
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            textArea.append(new String(buf, 0, len));

                            // Make sure the last line is always visible
                            textArea.setCaretPosition(textArea.getDocument().getLength());

                            // Keep the text area down to a certain character size
                            int idealSize = 5000;
                            int maxExcess = 1500;
                            int excess = textArea.getDocument().getLength() - idealSize;
                            if (excess >= maxExcess) {
                                textArea.replaceRange("", 0, excess);
                            }
                        }
                    });
                }
            } catch (IOException e) {
            }
        }
    }
    
    public abstract static class IOConsoleWindow extends ConsoleWindow {

        final protected JTextField inputField = new JTextField();
        
        public IOConsoleWindow() {
            super();
            
            getContentPane().add(inputField, BorderLayout.SOUTH);
            getContentPane().layout();
            pack();
            
            inputField.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    final String es = inputField.getText();
                    inputField.setText("");
                    SwingUtilities.invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            input(es);
                        }                        
                    });
                }                
            });
        }
        
        
        abstract public void input(String e);
    }
    
    public static class JavascriptConsoleWindow extends IOConsoleWindow {
        private final Context cx;
        private final ScriptableObject scope;

        public JavascriptConsoleWindow()  {
            super();
            
            setTitle("Javascript");
            
            cx = Context.enter();
            scope = cx.initStandardObjects();
            cx.exit();
            
        }

        public void exposeObject(String name, Object o) {
            //Object wrappedOut = Context.javaToJS(o, scope);
            ScriptableObject.putProperty(scope, name, o);        
            
        }
        
        @Override
        public void input(String e) {
            cx.enter();
            try {
                textArea.append("> " + e + "\n");
                Object result = cx.evaluateString(scope,e, "input", 1, null);
                textArea.append(result.toString() + "\n\n");
            }
            catch (Exception x) {
                System.err.println(x);
                x.printStackTrace();                
            }
            cx.exit();
        }
        
    }
    
}