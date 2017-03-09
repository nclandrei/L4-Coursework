package uk.ac.gla.dcs.dsms;

import java.util.ArrayList;
import java.util.List;

import org.terrier.structures.postings.BlockPosting;
import org.terrier.structures.postings.IterablePosting;
import org.terrier.matching.dsms.DependenceScoreModifier;

public class DiffAvgPosProxFeatureDSM extends DependenceScoreModifier {

	@Override
	protected double calculateDependence(IterablePosting[] ips, boolean[] okToUse, double[] phraseTermWeights, boolean SD) {
		int numberOfQueryTerms = okToUse.length;
		int numberOfPairs = 0;		
		double score = 0.0d;  
		
		List<Double> avgPosList = new ArrayList<>();
		for (int i=0; i< ips.length ; i++) {
			if (okToUse[i]) {
				BlockPosting bp = (BlockPosting) ips[i]; 
				int[]posList = bp.getPositions();
				double avg_pos = 0;
				for (int pos : posList) {
					avg_pos += pos;
				}
				avg_pos /= posList.length;
				avgPosList.add(avg_pos);
			}
		}
		
		for (int i=0; i< avgPosList.size()-1;  i++) {
			for (int j=i+1; j< avgPosList.size(); j++) {
				numberOfPairs +=1;
				score += Math.abs(avgPosList.get(j)-avgPosList.get(i));
			}
		}
		
		if (numberOfPairs == 0) {
			return 0.0d;
		}
		
		score /= numberOfPairs;	
		return score;
	}

	@Override
	protected double scoreFDSD(int match, int len) {
		throw new UnsupportedOperationException();
	}


	@Override
	public String getName() {
		return "DAPPF_DSM";
	}
	
}
