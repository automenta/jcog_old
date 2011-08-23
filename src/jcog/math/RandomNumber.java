/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.math;

/**
 *
 * @author seh
 */
public class RandomNumber {

    /** inclusive, so be careful with the max parameter */
    public static int getInt(int min, int max) {
        return min + ((int)Math.round(Math.random() * (max-min)));
    }

    public static double getDouble(double min, double max) {
        return min + (Math.random() * (max-min));
    }

    public static float getFloat(float min, float max) {
        return (float)(min + (Math.random() * (max-min)));
    }
}
