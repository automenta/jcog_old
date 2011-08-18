/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import java.util.HashMap;
import java.util.Map;
import javax.vecmath.Vector2f;
import javax.vecmath.Vector3f;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.swing.GraphView2D;
import jcog.spacegraph.shape.Rect;

/**
 *
 * @author seh
 */
public class FDLayout extends GraphViewProcess {

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
    public void refresh(GraphView2D graphView) {
                
        for (Atom a : graphView.getVisibleVertices()) {
            Rect r = graphView.getVertexShape(a);
            
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

    protected void applyCoulombsLaw(GraphView2D g) {
        final Vector2f dd = new Vector2f();
        final Vector2f repulsive = new Vector2f();
        
        for (final Atom a : g.getVisibleVertices()) {            
            for (final Atom b : g.getVisibleVertices()) {
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

    protected void applyHookesLaw(GraphView2D g) {
        for (HyperedgeSegment s : g.getVisibleEdges()) {
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
    
    protected void updateVelocityAndPosition(final GraphView2D gv, double dt)  {
        for (final Atom a : gv.getVisibleVertices()) {            
            final Vector2f f = new Vector2f(getForce(a));
            f.scale((float)dt);
            
            final Vector2f v = getVelocity(a);
            v.add(f);
            v.scale((float)damping);
            
            final Vector2f vt = new Vector2f(v);
            vt.scale((float)dt);
            
            final Vector2f pos = getPosition(a);
            pos.add(vt);
            
            gv.setTargetCenter(gv.getVertexShape(a), pos.x, pos.y, 0);
        }        
    }

    protected void attractToCenter(GraphView2D g) {
        for (final Atom a : g.getVisibleVertices()) {            
            Vector2f direction = new Vector2f(getPosition(a));
            direction.scale(-1.0f * (float)this.repulsion / 50.0f);
            applyForce(a, direction, true);
        }
        
    }
    
    public void fdLayoutToShapes(final GraphView2D g) {
        for (final Atom a : g.getVisibleVertices()) {
            final Vector2f v = getPosition(a);
            Rect tr = g.getVertexShape(a);
            g.setTargetCenter(tr, v.getX(), v.getY(), 0);
        }        
    }
            
    
    @Override
    protected void update(final GraphView2D g) {       
        applyCoulombsLaw(g);
        applyHookesLaw(g);
        attractToCenter(g);
        updateVelocityAndPosition(g,dt);
        
        fdLayoutToShapes(g);
    }

    @Override
    public boolean isReady(GraphView2D graphView) {
        return accumulated > graphView.param.getLayoutUpdatePeriod();
    }

    
}
