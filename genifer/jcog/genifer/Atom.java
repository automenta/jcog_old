package jcog.genifer;

/**
 * Atoms are opaque, symbolic names.
 */
public class Atom extends Constant {

    public final String name;

    public Atom(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Atom) {
            Atom a = (Atom) obj;
            return name.equals(a.name);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}
