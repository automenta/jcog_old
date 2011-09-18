/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.destin;

import java.util.Arrays;
import org.encog.ml.MLCluster;
import org.encog.ml.kmeans.KMeansCluster;
import org.encog.ml.kmeans.KMeansClustering;
import org.encog.neural.data.basic.BasicNeuralData;
import org.encog.neural.data.basic.BasicNeuralDataSet;

/**
 *
 * @author seh
 */
public class TestDestin {
    
    public static class DestinNode {
        KMeansClustering clustering;
        
        public void observe(double[] o) {
            
        }
    }
    
    	public static final double[][] DATA = {
        {28,15,22},
        {16,15,32},
        {32,20,44},
        {1,2,3},
        {3,2,1} };
	
    public static void main (String args[]){
                
        BasicNeuralDataSet set = new BasicNeuralDataSet();
        
        for(int i=0;i<DATA.length;i++)
        {
        	set.add(new BasicNeuralData(DATA[i]));
        }

        KMeansClustering kmeans = new KMeansClustering(2,set);
        
        kmeans.iteration(100);
        System.out.println("Final WCSS: " + kmeans.getWCSS());
                       
        int i = 1;
        for(MLCluster cluster: kmeans.getClusters())
        {
            KMeansCluster kmc = (KMeansCluster)cluster;
        	System.out.println("*** Cluster " + (i++) + " ***");
                System.out.println(cluster.getData());
            System.out.println(kmc.getCentroid().getCenters().length);
                
        }       
    }
}
