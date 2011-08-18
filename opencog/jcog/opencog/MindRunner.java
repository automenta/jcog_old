/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog;

import com.google.common.base.Predicate;

/**
 *
 * @author seh
 */
public class MindRunner implements Runnable {

    public boolean running = false;
    private double period;
    private final OCMind mind;
    private Thread thread;
    
    /**
     * executes a per-cycle operation for a given number of cycles
     * 
     * @param mind
     * @param period
     * @param numCycles
     * @param perCycle 
     */
    public MindRunner(OCMind mind, double period, int numCycles, Predicate<OCMind> perCycle) {
        super();
        
        this.mind = mind;
        this.period = period;
        
        for (int i = 0; i < numCycles; i++) {
            mind.cycle();
            if (perCycle!=null)
                if (!perCycle.apply(mind))
                    break;
        }
        
    }

    /**
     * when created, starts mindrunner thread
     * @param mind
     * @param period 
     */
    public MindRunner(OCMind mind, double period) {
        this(mind, period, false);
    }
    
    public MindRunner(OCMind mind, double period, boolean newThread) {
        this(mind, period, 0, null);
                  
        if (newThread)
            restart();       
        else {
            running = true;
            run();
        }
    }

    public double getPeriod() {
        return period;
    }

    public void setPeriod(double period) {
        this.period = period;
    }
    
    public void restart() {
        if (!running) {
            if (this.thread == null)
                this.thread = new Thread(this);
            
            running = true;
            thread.start();
        }
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
