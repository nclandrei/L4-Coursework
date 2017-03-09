package uk.ac.gla.dcs.dsms;

import org.terrier.structures.postings.BlockPosting;
import org.terrier.structures.postings.IterablePosting;
import org.terrier.matching.dsms.DependenceScoreModifier;

/**
 * This class implements the min_dist (a, b, D) from the proximity measures
 * section in Ronan Cummins and Colm O'Riordan's paper.
 * @author 2147392n
 */
public class MinDistProximityFeatureDSM extends DependenceScoreModifier {

    @Override
    protected double calculateDependence(IterablePosting[] ips, boolean[] okToUse,
                                         double[] phraseTermWeights, boolean SD) {
        double finalScore = 0.0d;
        int pairCounter = 0;
        int len = ips.length;

        for (int i = 0; i< len - 1 ; i++) {
            if (okToUse[i]) {
                for (int j = i + 1; j < len; j++) {
                    if (okToUse[j]) {
                        pairCounter +=1;
                        BlockPosting firstBlockPosting = (BlockPosting) ips[i];
                        int[] firstBlockPostingPositionsList = firstBlockPosting.getPositions();
                        BlockPosting secondBlockPosting = (BlockPosting) ips[j];
                        int[] secondBlockPostingPositionsList = secondBlockPosting.getPositions();
                        finalScore += getMinDistance(firstBlockPostingPositionsList, secondBlockPostingPositionsList);
                    }
                }
            }
        }

        return (pairCounter == 0) ? 0.0d : finalScore/pairCounter;
    }



    @Override
    protected double scoreFDSD(int matchingNGrams, int docLength) {
        throw new UnsupportedOperationException();
    }


    @Override
    public String getName() {
        return "Min_Dist_Proximity_Feature_DSM";
    }

    private double getMinDistance(int[] firstBlockPostingPositionsList, int[] secondBlockPostingPositionsList) {
        double min = Double.MAX_VALUE;

        for (int firstElem : firstBlockPostingPositionsList) {
            for (int secondElem : secondBlockPostingPositionsList) {
                double distance = Math.abs(firstElem - secondElem);
                if (min > distance) {
                    min = distance;
                }
            }
        }

        return min;
    }
}
