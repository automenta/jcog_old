package jcog.spacegraph.shape.util;

import jcog.spacegraph.math.linalg.Vec3f;
import jcog.spacegraph.shape.Rect;

public class InRect extends Vec3f {

    private final Rect rect;
    private final float dx;
    private final float dy;

    public InRect(Rect r, float dx, float dy) {
        super();
        this.rect = r;
        this.dx = dx;
        this.dy = dy;
    }

    @Override
    public float x() {
        return rect.getCenter().x() + dx * rect.getScale().x();
    }

    @Override
    public float y() {
        return rect.getCenter().y() + dy * rect.getScale().y();
    }

    @Override
    public float z() {
        return 0;
    }
}
