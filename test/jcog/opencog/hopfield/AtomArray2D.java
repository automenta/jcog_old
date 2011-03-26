/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.opencog.hopfield;

import java.util.ArrayList;
import java.util.List;
import jcog.math.RandomNumber;
import jcog.opencog.Atom;
import jcog.opencog.AtomTypes;
import jcog.opencog.DefaultOCMind;
import jcog.opencog.atom.AttentionValue;

/**
 *
 * @author seh
 */
public class AtomArray2D {

    public final List<Atom> atoms;
    private final DefaultOCMind atomspace;
    public final int width;
    public final int height;

    public AtomArray2D(DefaultOCMind s, String namePrefix, int width, int height) {
        super();

        this.atomspace = s;
        this.width = width;
        this.height = height;

        atoms = new ArrayList(width * height);

        // Create nodes for presenting the pattern to be learned:
        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                Atom a = s.addVertex(AtomTypes.ConceptNode, namePrefix + "_" + i + "_" + j);

                // We don't want the forgetting process to remove these perception atoms
                s.setVLTI(a, AttentionValue.NonDisposable);

                atoms.add(a);
            }
        }

    }

    public Atom getAtom(int x, int y) {
        if ((x < 0) || (y < 0) || (x >= width) || (y >= height)) {
            return null;
        }
        return atoms.get(y * width + x);
    }

    public Atom getRandomNode() {
        int i = RandomNumber.getInt(0, atoms.size() - 1);
        return atoms.get(i);
    }
}
