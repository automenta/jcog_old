package jcog.nars.reason;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;
import jcog.nars.reason.io.Experience;

public class DefaultNAR implements NAR {

    /** System clock, relatively defined to guaranttee the repeatability of behaviors */
    private long now;
    /** Timer for fixed distance walking */
    private long stoper;
    /** Flag for running continously */
    private boolean running;
    private Memory memory;
    private Experience experienceStream;
    private Logger log;
    private List<NARObserver> observers = new LinkedList();
    private NARParams params;

    public DefaultNAR(NARParams params) {
        super();

        this.params = params;

        log = Logger.getLogger(toString());

        this.memory = new Memory(params);

        start();
    }

    public DefaultNAR() {
        this(new DefaultNARParams());
    }

    public void start() {
        experienceStream = new Experience(getMemory());

        reset();
    }

    public void reset() {
        stoper = 0;
        now = 0;
        running = false;
//        Stamp.init();
        getMemory().init();

    }

    /**
     * Walk a fixed number of steps or continously. Called from MainWindow only.
     * @param i The number of steps of inference, or infinite if negative
     */
    public void setStoper(long i) {
        if (i < 0) {
            running = true;
            stoper = 0;
        } else {
            running = false;
            stoper = i;
        }
    }

    public void cycle(int cycles) {
        for (int i = 0; i < cycles; i++)
            cycle();        
    }
    
    /**
     * A clock tick. Run one working cycle or read input. Called from NARS only.
     */
    public void cycle() {
//        if (stoper == 0) {
//            stoper = experienceStream.loadLine();
//        }
//        
//        if (running || (stoper > 0)) {
        now++;

//            if (getLog().isLoggable(Level.INFO))
//            	getLog().info(" --- " + now + " ---\n");

        getMemory().cycle(/*now*/);

        //this will be responsible for when previously calling mainWindow.tickTimer();
        for (NARObserver o : observers) {
            o.onCycle(this);
        }


//            if (stoper > 0) {
//                stoper--;
//            }
//        }
    }

    private NARParams getParams() {
        return params;
    }

    /**
     * Get the current time from the clock
     * Called in nars.entity.Stamp
     * @return The current time
     */
    public long getTime() {
        return now;
    }

    public Memory getMemory() {
        return memory;
    }

    public Experience getExperience() {
        return experienceStream;
    }

    public Logger getLog() {
        return log;
    }

    @Override
    public long getNow() {
        return now;
    }
}
