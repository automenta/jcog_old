package jcog.opencog;

@Deprecated public interface Predicate<X> {
    //TODO use google library's Predicate
    public boolean isTrue(X x);
}
