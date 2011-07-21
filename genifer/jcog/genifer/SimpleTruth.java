/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.genifer;

/**
 *
 * @author seh
 */
public class SimpleTruth implements Truth {
    public double prob;
    public double confidence;

    public SimpleTruth(double p, double confidence) {
        this.prob = p;
        this.confidence = confidence;
    }

    @Override
    public String toString() {
        return prob + ", " + confidence;
    }

    @Override
    public double getConfidence() {
        return confidence;
    }

    @Override
    public double getProbability() {
        return prob;
    }
    
}
