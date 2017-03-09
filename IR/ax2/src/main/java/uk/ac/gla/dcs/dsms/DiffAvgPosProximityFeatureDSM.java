package uk.ac.gla.dcs.dsms;

import java.util.ArrayList;
import java.util.List;

import org.terrier.structures.postings.BlockPosting;
import org.terrier.structures.postings.IterablePosting;
import org.terrier.matching.dsms.DependenceScoreModifier;

/**
 * This class implements the 
 * @author 2147392n
 */
public class DiffAvgPosProximityFeatureDSM extends DependenceScoreModifier {

	@Override
	protected double calculateDependence(IterablePosting[] ips, boolean[] okToUse,
										 double[] phraseTermWeights, boolean SD) {
		int numberOfPairs = 0;
		double finalScore = 0.0d;
		int len = ips.length;
		List<Double> avgPosList = new ArrayList<>();

		for (int i = 0; i < len; i++) {
			if (okToUse[i]) {
				BlockPosting blockPosting = (BlockPosting) ips[i];
				int[] positionsList = blockPosting.getPositions();
				double avg_pos = 0;
				for (int pos : positionsList) {
					avg_pos += pos;
				}
				avg_pos /= positionsList.length;
				avgPosList.add(avg_pos);
			}
		}
		
		for (int i = 0; i < avgPosList.size() - 1;  i++) {
			for (int j = i + 1; j < avgPosList.size(); j++) {
				numberOfPairs += 1;
				finalScore += Math.abs(avgPosList.get(j) - avgPosList.get(i));
			}
		}

		return (numberOfPairs == 0) ? 0.0d : finalScore / numberOfPairs;
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
