/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.spacegraph.ui;

import jcog.spacegraph.math.linalg.Vec4f;
import jcog.spacegraph.shape.TextRect;
import javax.media.opengl.GL2;

/**
 *
 * @author me
 */
public class TextButton extends Button {

    private String text;
    private TextRect tr;

    private Vec4f textColor = new Vec4f(0, 0, 0, 1f);
    
    public TextButton(String label) {
        super();
        setText(label);
    }

    public void setText(String newText) {
        this.text = newText;
        tr = new TextRect(text);
        tr.setTextColor(textColor);
    }

    @Override
    protected void drawFront(GL2 gl) {
        super.drawFront(gl);
        tr.draw(gl);        
    }
    
    
    
}
