/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog;

import java.util.LinkedList;

/**
 *
 * @author seh
 */
public class TaskQueue extends MindAgent {

    LinkedList<Runnable> toRun = new LinkedList();

    public TaskQueue(double period) {
        super();
        setPeriod(period);
    }

    @Override
    protected void run(OCMind mind) {
        if (toRun.size() > 0) {
            Runnable r = toRun.pop();
            r.run();
        }
    }

    public void queue(Runnable r) {
        toRun.add(r);
    }
}
