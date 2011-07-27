/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.agent;

import java.util.LinkedList;
import jcog.opencog.MindAgent;
import jcog.opencog.OCMind;

/**
 *
 * @author seh
 */
public class ExecuteRunnables extends MindAgent {

    LinkedList<Runnable> toRun = new LinkedList();

    public ExecuteRunnables(double period) {
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
