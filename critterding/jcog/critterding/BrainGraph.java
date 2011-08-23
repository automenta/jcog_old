/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.critterding;

import com.syncleus.dann.graph.AbstractDirectedEdge;
import com.syncleus.dann.graph.MutableAdjacencyGraph;
import jcog.critterding.BrainGraph.SynapseEdge;

/**
 *
 * @author seh
 */
public class BrainGraph extends MutableAdjacencyGraph<AbstractNeuron, SynapseEdge> {

    public static class SynapseEdge extends AbstractDirectedEdge<AbstractNeuron> {
        
        public final Synapse synapse;

        public SynapseEdge(Synapse s, AbstractNeuron source) {
            super(source, s.inputNeuron);
            this.synapse = s;
        }
        
    }
    
    private final Brain brain;

    public BrainGraph(Brain b) {
        super();

        this.brain = b;
        
        update();
    }

    protected void update() {
        clear();

        for (SenseNeuron sn : brain.getSense()) {
            add(sn);
        }
        for (MotorNeuron mn : brain.getMotor()) {
            add(mn);
        }
        for (InterNeuron in : brain.getNeuron()) {
            add(in);
        }

        for (InterNeuron in : brain.getNeuron()) {
            for (Synapse s : in.getSynapses()) {
                AbstractNeuron pred = s.inputNeuron;
                add(new SynapseEdge(s, in));
            }            
        }
    }


}
