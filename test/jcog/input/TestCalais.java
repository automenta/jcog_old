/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.input;

import java.io.IOException;
import jcog.input.calais.CalaisEntity;
import jcog.input.calais.CalaisTag;
import jcog.input.calais.CalaisTopic;
import mx.bigdata.jcalais.CalaisClient;
import mx.bigdata.jcalais.CalaisObject;
import mx.bigdata.jcalais.CalaisResponse;
import mx.bigdata.jcalais.rest.CalaisRestClient;

/**
 *
 * @author seh
 */
public class TestCalais {
    public static void main(String[] args) {
        CalaisClient client = new CalaisRestClient("vu5dvm8fvzxtphrmwsaubaap");
        try {
            CalaisResponse response = client.analyze("Prosecutors at the trial of former Liberian President Charles Taylor " 
               + " hope the testimony of supermodel Naomi Campbell " 
               + " will link Taylor to the trade in illegal conflict diamonds, "
               + " which they say he used to fund a bloody civil war in Sierra Leone.");
            
            for (CalaisObject entity : response.getEntities()) {
                System.out.println(new CalaisEntity(entity));
            }
            for (CalaisObject topic : response.getTopics()) {
                System.out.println(new CalaisTopic(topic));
            }
            for (CalaisObject tags : response.getSocialTags()){
                System.out.println(new CalaisTag(tags));
            }
            
//            System.out.println(response);
//            System.out.println(response.getInfo());
//            System.out.println(response.getMeta());
//            System.out.println(response.getEntities());
//            System.out.println(response.getRelations());
//            System.out.println(response.getSocialTags());
//            System.out.println(response.getTopics());
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
