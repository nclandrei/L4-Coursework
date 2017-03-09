package uk.ac.gla.dcs.dsms;

import java.util.ArrayList;
import java.util.List;

import org.terrier.structures.postings.BlockPosting;
import org.terrier.structures.postings.IterablePosting;
import org.terrier.matching.dsms.DependenceScoreModifier;

/**
 * This class implements the diff_avg_pos (a, b, D) from the proximity measures
 * section in Ronan Cummins and Colm O'Riordan's paper.
 * @author 2147392n
 */
public class DiffAvgPosProximityFeatureDSM extends DependenceScoreModifier {

	@Override
	protected double calculateDependence(IterablePosting[] ips, boolean[] okToUse,
										 double[] phraseTermWeights, boolean SD) {
		double finalScore = 0.0d;
		int len = ips.length;
        int pairCounter = 0;
		List<Double> avgPosList = new ArrayList<>();

		for (int i = 0; i < len; i++) {
			if (okToUse[i]) {
				BlockPosting blockPosting = (BlockPosting) ips[i];
				int[] positionsList = blockPosting.getPositions();
				double averagePosition = 0;
				for (int pos : positionsList) {
					averagePosition += pos;
				}
				averagePosition /= positionsList.length;
				avgPosList.add(averagePosition);
			}
		}

		int avgPosListSize = avgPosList.size();
		
		for (int i = 0; i < avgPosListSize - 1;  i++) {
			for (int j = i + 1; j < avgPosListSize; j++) {
				pairCounter += 1;
				finalScore += Math.abs(avgPosList.get(j) - avgPosList.get(i));
			}
		}

		return (pairCounter == 0) ? 0.0d : finalScore / pairCounter;
	}

	@Override
	protected double scoreFDSD(int match, int len) {
		throw new UnsupportedOperationException();
	}

	@Override
	public String getName() {
		return "Diff_Avg_Pos_Proximity_Feature_DSM";
	}
	
}
