/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import javax.vecmath.Vector2f;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.swing.GraphView2D;
import jcog.spacegraph.shape.Rect;

/**
 *
 * @author seh
 */
public class FDLayout extends GraphViewProcess {
    private MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment> digraph;

    public class VertexBody {
        public final Vector2f position;
        public final Vector2f velocity;
        public final Vector2f force;

        public VertexBody(Vector2f position, Vector2f velocity, Vector2f force) {
            this.position = position;
            this.velocity = velocity;
            this.force = force;
        }       
        
    }
    
    Map<Atom, VertexBody> bodies = new HashMap();
    
    double stiffness = 0;
    double repulsion = 5.0;
    double damping = 0.1;
    float rad = 4.0f;
    float idealLength = 2f;

    double dt = 0.1;
    
    public FDLayout() {
        super();
    }

    @Override
    public void reset(GraphView2D graphView) {
        
        digraph = graphView.getMind().foldHypergraphEdges();

        graphView.edgeCurve.clear();
        for (HyperedgeSegment fe : digraph.getEdges()) {
            graphView.addEdge(fe);
        }
        
        graphView.atomRect.clear();
        for (Atom a : digraph.getNodes()) {
            Rect r = graphView.addVertex(a);
            
            final float x = r.getCenter().x();
            final float y = r.getCenter().y();
            final float z = r.getCenter().z();
            graphView.setTargetCenter(r, x, y, z);
        }
    }

    public VertexBody getBody(Atom a) {
        VertexBody v = bodies.get(a);
        if (v == null) {
            //TODO use random position within a given radius
            v = new VertexBody(new Vector2f(RandomNumber.getFloat(-rad, rad), RandomNumber.getFloat(-rad, rad)), new Vector2f(), new Vector2f());
            bodies.put(a, v);
        }
        return v;
    }
    
    public Vector2f getPosition(Atom a) {
        return getBody(a).position;
    }
    
    public Vector2f getForce(Atom a) {
        return getBody(a).force;
    }
    
    public Vector2f getVelocity(Atom a) {
        return getBody(a).velocity;
    }
    
    public void applyForce(Atom a, Vector2f force, boolean positive) {
        if (positive)
            getForce(a).add(force);
        else
            getForce(a).sub(force);
    }
    
//    public Vector2f getForce(Atom a, Atom b, double factor) {
//        Vector2f aa = getPosition(a);
//        Vector2f bb = getPosition(b);
//        bb.sub(aa);
//        bb.scale((float)factor);
//        return bb;
//    }    

    protected void applyCoulombsLaw() {
        final Vector2f dd = new Vector2f();
        final Vector2f repulsive = new Vector2f();
        
        for (final Atom a : digraph.getNodes()) {            
            for (final Atom b : digraph.getNodes()) {
                if (a == b) continue;
                    
                final Vector2f va = getPosition(a);
                final Vector2f vb = getPosition(b);

                dd.sub(va, vb);

                final double distance = dd.length() + 1.0;
                dd.normalize();

                repulsive.scale((float)(this.repulsion / (distance * distance * 0.5)), dd);

                //System.out.println("coloumb: " + repulsive);
                applyForce(a, repulsive, true);
                applyForce(b, repulsive, false);
            }
        }
        
    }
        
    public float getIdealLength(HyperedgeSegment s) {
        return idealLength;
    }

    protected void applyHookesLaw() {
        for (HyperedgeSegment s : digraph.getEdges()) {
            final Atom a = s.getSourceNode();
            final Atom b = s.getDestinationNode();
            final Vector2f pa = getPosition(a);
            final Vector2f pb = getPosition(b);
            
            final Vector2f d = new Vector2f(pb);
            d.sub(pa);
            
            double displacement = getIdealLength(s) - d.length();
            
            d.normalize();
            final Vector2f direction = d;
            
            d.scale((float)(stiffness * displacement * 0.5));
            //System.out.println("hook: " + d);
            applyForce(a, d, false);
            applyForce(b, d, true);
        }
    }
    
    protected void updateVelocityAndPosition(double dt)  {
        for (final Atom a : digraph.getNodes()) {            
            final Vector2f f = new Vector2f(getForce(a));
            f.scale((float)dt);
            
            final Vector2f v = getVelocity(a);
            v.add(f);
            v.scale((float)damping);
            
            final Vector2f vt = new Vector2f(v);
            vt.scale((float)dt);
            
            getPosition(a).add(vt);
            
        }        
    }

    protected void attractToCenter() {
        for (final Atom a : digraph.getNodes()) {            
            Vector2f direction = new Vector2f(getPosition(a));
            direction.scale(-1.0f * (float)this.repulsion / 50.0f);
            applyForce(a, direction, true);
        }
        
    }
            
    
    @Override
    protected void update(GraphView2D g) {

        
        //cleanup vertices
        //List<Atom> toRemove = new LinkedList();
//        for (Atom a : digraph.getNodes()) {
//            if (!g.atomRect.containsKey(a))
//                toRemove.add(a);
//        }
//        for (Atom a : toRemove) {
//            bodies.remove(a);
//            g.atomRect.remove(a);
//        }
        
        applyCoulombsLaw();
        applyHookesLaw();
        attractToCenter();
        updateVelocityAndPosition(dt);

        //        // stop simulation when energy of the system goes below a threshold
//        if (t.totalEnergy() < 0.1)
//        {
//                clearInterval(t.intervalId);
//                t.intervalId = null;
//                if (typeof(done) !== 'undefined') { done(); }
//        }

        
        for (Atom a : digraph.getNodes()) {
            final Vector2f v = getPosition(a);
            Rect tr = g.addVertex(a);
            g.setTargetCenter(tr, v.getX(), v.getY(), 0);
        }
    }

    @Override
    public boolean isReady(GraphView2D graphView) {
        return accumulated > graphView.param.getLayoutUpdatePeriod();
    }

    
}
