/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input.calais;

import mx.bigdata.jcalais.CalaisClient;
import mx.bigdata.jcalais.CalaisResponse;
import mx.bigdata.jcalais.rest.CalaisRestClient;

/**
 *
 * @author seh
 */
public class Calais {
    
    public final static String apiKey = "vu5dvm8fvzxtphrmwsaubaap";
    
    public static CalaisResponse getCalais(String text) throws Exception {
        CalaisClient client = new CalaisRestClient(apiKey);
        CalaisResponse response = client.analyze(text); 
        return response;
    }
}
