/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.spacegraph.shape;

import jcog.spacegraph.math.linalg.Vec3f;
import jcog.spacegraph.math.linalg.Vec4f;
import com.sun.opengl.util.awt.TextRenderer;
import java.awt.Font;
import java.awt.geom.Rectangle2D;
import javax.media.opengl.GL2;

/**
 *
 * @author seh
 */
public class TextRect extends Rect {

    private TextRenderer textRenderer;
    private float textScaleFactor;
    private String text;
    private boolean useVertexArrays = false;
    private Vec4f textColor = new Vec4f(1f,1f,1f, 1f);
    private Rectangle2D bounds;

    boolean textChanged;
    
    public TextRect(String initialText) {
        super();
        setText(initialText);
    }

    public TextRect(TextRenderer textRenderer, String initialText) {
        this(initialText);
        this.textRenderer = textRenderer;
    }

    public void setText(String text) {
        this.text = text;
        textChanged = true;
    }
    
    public static TextRenderer newTextRenderer(Font font) {
        TextRenderer textRenderer = new TextRenderer(font);
        return textRenderer;
    }

    @Override
    public void draw(GL2 gl) {
        if (isFilled())
            super.draw(gl);

        if (textRenderer == null) {
            textRenderer = new TextRenderer(new Font("Arial", Font.PLAIN, 72));
            textRenderer.setSmoothing(false);
            textRenderer.setUseVertexArrays(useVertexArrays);
            //bounds = textRenderer.getBounds("Bottom");            
        }
        
        if (textChanged) {
            bounds = textRenderer.getBounds(text);
            textChanged = false;
        }


        textScaleFactor = 0.025f;

        gl.glPushMatrix();
        transform(gl);

        // Now draw the overlaid text. In this setting, we don't want the
        // text on the backward-facing faces to be visible, so we enable
        // back-face culling; and since we're drawing the text over other
        // geometry, to avoid z-fighting we disable the depth test. We
        // could plausibly also use glPolygonOffset but this is simpler.
        // Note that because the TextRenderer pushes the enable state
        // internally we don't have to reset the depth test or cull face
        // bits after we're done.
        textRenderer.begin3DRendering();
        gl.glEnable(GL2.GL_DEPTH_TEST);
        gl.glEnable(GL2.GL_CULL_FACE);

        // Note that the defaults for glCullFace and glFrontFace are
        // GL_BACK and GL_CCW, which match the TextRenderer's definition
        // of front-facing text.
        float w = (float) bounds.getWidth();
        float h = (float) bounds.getHeight();
        textRenderer.setColor(textColor.x(), textColor.y(), textColor.z(), 1f);
        textRenderer.draw3D(text,
            w / -2.0f * textScaleFactor,
            h / -2.0f * textScaleFactor,
            0.1f,
            textScaleFactor);
        textRenderer.end3DRendering();

        gl.glPopMatrix();


    }

    public void setTextColor(Vec4f color) {
        this.textColor = color;
    }
}
