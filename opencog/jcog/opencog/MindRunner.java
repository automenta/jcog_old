/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author seh
 */
public class MindRunner implements Runnable {

    public boolean running = true;
    private double period;
    private final OCMind mind;
    private final Thread thread;
    
    public MindRunner(OCMind mind, double period) {
        super();
        
        this.mind = mind;
        this.period = period;
        
        this.thread = new Thread(this);
        
        thread.start();
    }

    public double getPeriod() {
        return period;
    }

    public void setPeriod(double period) {
        this.period = period;
    }
        
    public void stop() {
        running = false;
    }

    @Override
    public void run() {
        mind.logger.info(this + " started");
        while (running) {
            
            try {
                mind.cycle();
                
                Thread.sleep((long)(getPeriod() * 1000.0));
                
            } catch (InterruptedException ex) {
            }
        }
        mind.logger.info(this + " finished");
    }
    
    
}
