package jcog.opencog;

public interface Operation<C, X> {

    /**
     * Always called after isApplicable, so isApplicable can safely cache
     * @param x
     * @return true = continue visiting and applying, false = prematurely terminate the visit
     */
    public boolean operate(C context, X x);
}
