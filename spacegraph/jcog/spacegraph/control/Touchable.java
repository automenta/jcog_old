/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.spacegraph.control;

import jcog.spacegraph.math.linalg.Vec2f;

/**
 *
 * @author me
 */
public interface Touchable {
    public boolean isTouchable();
    public boolean intersects(Vec2f world);
    public void onTouchChange(Pointer pointer, boolean touched);
}
