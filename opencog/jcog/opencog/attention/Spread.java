package jcog.opencog.attention;

import java.util.HashSet;
import java.util.List;
import java.util.Set;


public class Spread {

////	short spreadThreshold;
////	short stealingLimit;
////	float importanceSpreadingMultiplier;
////	RecentLong amountSpread;
//
//	public static void spread(FloatMap strength, float maxSpread, float spreadRate, String[] alongTypes ) {
//		Set<String> includeTypes = new HashSet();
//		for (String s : alongTypes) {
//			includeTypes.add(s);
//		}
//		
//		AtomNet net = strength.getNet();
//		
//		for (String i : net.getAtoms()) {
//			String t = net.getType(i);
//			if (includeTypes.contains(t)) {
//				
//				List<String> linked = net.getOutgoing(i);
//				if (linked.size() == 0)
//					continue;
//				
//				float totalStrength = 0;
//				for (String s : linked) {
//					totalStrength += strength.get(s);
//				}
//				float meanStrength = totalStrength / linked.size();
//				//float spreadableStrength = totalStrength - (meanStrength * linked.size());
//				float nextValue = meanStrength;// + Math.min(maxSpread, (spreadableStrength / ((float)linked.size()) )); 
//				for (String s : linked) {
//					float p = strength.get(s);
////					if (p < meanStrength) {
////						p = Math.min(meanStrength, p + maxSpread);
////					}
//					
//					float a = ((1.0f - spreadRate) * p) + (spreadRate * nextValue);
//					strength.set(s, a);
//				}
//			}
//		}
//		
//	}

}
