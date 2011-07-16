/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.spacegraph.shape;

import javax.media.opengl.GL2;
import jcog.spacegraph.math.linalg.Vec2f;
import jcog.spacegraph.math.linalg.Vec3f;

/**
 *
 * @author seh
 */
public class TrapezoidLine extends Spatial implements Drawable {

    private final Rect aRect;
    private final Rect bRect;
    float sourceWidth, endWidth;
    private float cr, cg, cb;
    //private final TextRect labelRect;

    public TrapezoidLine(Rect aRect, Rect bRect, float sourceWidth, float endWidth) {
        this.aRect = aRect;
        this.bRect = bRect;
        setSourceWidth(sourceWidth);
        setEndWidth(endWidth);
    }

    public void setWidths(float[] envelope) {
        if ((envelope.length > 2) || (envelope.length < 1)) {
            System.out.println(this + " supports envelope of length 1 or 2 widths");
            return;
        }
        if (envelope.length == 1) {
            this.sourceWidth = envelope[0];
            this.endWidth = envelope[0];
        }
        else if (envelope.length == 2) {
            this.sourceWidth = envelope[0];
            this.endWidth = envelope[1];            
        }
    }
    public void setSourceWidth(float sourceWidth) {
        this.sourceWidth = sourceWidth;
    }

    public void setEndWidth(float endWidth) {
        this.endWidth = endWidth;
    }    

    @Override
    public void draw(GL2 gl) {
        gl.glEnable(GL2.GL_DEPTH_TEST);

        gl.glPushMatrix();
        transform(gl);

        gl.glEnable(gl.GL_LINE_SMOOTH);

        gl.glColor3f(cr, cg, cb);
        
        gl.glBegin(gl.GL_TRIANGLES);
        {
            Vec2f direction = new Vec2f(bRect.center.x() - aRect.center.x(), bRect.center.y() - aRect.center.y());
            direction.normalize();
            direction.set(-direction.y(), direction.x());
            
            final float s1x = aRect.center.x()+direction.x() * sourceWidth;
            final float s1y = aRect.center.y()+direction.y() * sourceWidth;
            final float s2x = aRect.center.x()-direction.x() * sourceWidth;
            final float s2y = aRect.center.y()-direction.y() * sourceWidth;
            float sz = aRect.center.z();

            final float t1x = bRect.center.x()+direction.x() * endWidth;
            final float t1y = bRect.center.y()+direction.y() * endWidth;
            final float t2x = bRect.center.x()-direction.x() * endWidth;
            final float t2y = bRect.center.y()-direction.y() * endWidth;
            float tz = bRect.center.z();

            gl.glVertex3f(s1x, s1y, sz);
            gl.glVertex3f(s2x, s2y, sz);
            gl.glVertex3f(t2x, t2y, tz);
            
            gl.glVertex3f(t2x, t2y, tz);
            gl.glVertex3f(t1x, t1y, tz);
            gl.glVertex3f(s1x, s1y, sz);
        }
        gl.glEnd();
        gl.glPopMatrix();

    }

    public void setColor(Vec3f color) {
        this.cr = color.x();
        this.cg = color.y();
        this.cb = color.z();
    }
    public void setColor(float cr, float cg, float cb) {
        this.cr = cr;
        this.cg = cg;
        this.cb = cb;
    }

    

}
