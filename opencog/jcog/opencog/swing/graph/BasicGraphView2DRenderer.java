/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.swing.graph;

import com.sun.opengl.util.awt.TextRenderer;
import java.awt.Color;
import java.awt.Font;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomType.UnorderedLink;
import jcog.opencog.OCMind;
import jcog.opencog.atom.TruthValue;
import jcog.opencog.swing.GraphView2D;
import jcog.spacegraph.shape.Rect;
import jcog.spacegraph.shape.TextRect;
import jcog.spacegraph.shape.TrapezoidLine;

/**
 *
 * @author seh
 */
public class BasicGraphView2DRenderer implements GraphView2DRenderer {
    final TextRenderer textRenderer = TextRect.newTextRenderer(new Font("Arial", Font.PLAIN, 32));

    @Override
    public Rect newVertex(OCMind mind, Atom v) {
        final String name = mind.getName(v);
        Rect r = new TextRect(textRenderer, name);
        
        float rr = 10f;
        r.getCenter().set(RandomNumber.getFloat(-rr, rr), RandomNumber.getFloat(-rr, rr), 0);
        
        return r;
    }

    @Override
    public void updateVertex(GraphView2D gv, final Atom vertex, final Rect r) {
        final OCMind mind = gv.getMind();
        final GraphView2D.GraphViewModel param = gv.param;
        final float vertexScale = (float) param.getDouble("VertexScale");
        
        if (vertex == null) {
            System.out.println("vertex = null");
            System.exit(1);
        }
        if (!mind.containsAtom(vertex)) {
            System.out.println("vertex not contained in mind " + vertex);
            Thread.dumpStack();
            System.exit(1);            
        }
        if (mind.getAttention(vertex) == null) {
            System.out.println("vertex attention = null");
            System.exit(1);            
        }
        
        final float sti = (float) mind.getNormalizedSTI(vertex);
        float sx = 0.1F + (float) (sti) * vertexScale;
        gv.setTargetScale(r, sx, sx, 1.0f);
        String n = mind.getName(vertex);
        if (n == null) {
            String type = mind.getTypeName(vertex);
            if (type != null) {
                n = type.toString();
            } else {
                n = "UNKNOWN";
            }
        }
        final float hue = ((Math.abs(n.toString().hashCode())) % 100) / 100.0F;
        final Color h = Color.getHSBColor(hue, 0.85F, sti * 0.5F + 0.5F);
        float[] hRGB = h.getColorComponents(null);
        r.getBackgroundColor().set(hRGB[0], hRGB[1], hRGB[2], 1.0F);
        r.setFilled(true);
    }

    @Override
    public TrapezoidLine newEdge(OCMind mind, Atom e, Atom source, Atom target, Rect sourceRect, Rect targetRect) {
        TrapezoidLine c = new TrapezoidLine(sourceRect, targetRect, 0.1F, 0.05F);
        return c;
    }

    @Override
    public void updateEdge(GraphView2D gv, Atom edge, TrapezoidLine c) {
        final OCMind mind = gv.getMind();
        final GraphView2D.GraphViewModel param = gv.param;
        float rti = (float) mind.getNormalizedSTI(edge);
        final TruthValue tv = mind.getTruth(edge);
        final float w = 1.0F + (float) tv.getMean() * 4.0F;
        final float edgeWidthScale = (float) param.getDouble("EdgeWidthScale") * (1.0F + rti);
        final float edgeRatio = UnorderedLink.class.isAssignableFrom(mind.getType(edge)) ? 1.0F : 3.0F;
        float[] curveProfile = new float[]{w * edgeWidthScale, w * (edgeWidthScale / edgeRatio)};
        c.setWidths(curveProfile);
        c.setZOffset(-0.1f);
        
        final float v = 0.7F + 0.3F * (float) mind.getTruth(edge).getMean();
        String type = mind.getTypeName(edge);
        final float hue = ((Math.abs(type.hashCode())) % 100) / 100.0F;
        final Color h = Color.getHSBColor(hue, 0.85F, v);
        float[] hRGB = h.getColorComponents(null);
        c.getColor().set(v * hRGB[0], v * hRGB[1], v * hRGB[2]);
    }
    
}
