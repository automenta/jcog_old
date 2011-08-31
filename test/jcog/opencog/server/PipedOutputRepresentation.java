/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.server;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import org.restlet.data.MediaType;
import org.restlet.representation.OutputRepresentation;

/**
 *
 * @author seh
 */
public class PipedOutputRepresentation extends OutputRepresentation {
    private final PipedInputStream stream;
    final int bufferSize = 8;

    public PipedOutputRepresentation(PipedInputStream pi) {
        super(MediaType.TEXT_PLAIN);
        this.stream = pi;
    }

    @Override
    public void write(OutputStream realOutput) throws IOException {
        byte[] b = new byte[bufferSize];
        int read;
        while ((read = stream.read(b)) != -1) {
            realOutput.write(b, 0, read);
            realOutput.flush();
        }
    }
    
}
