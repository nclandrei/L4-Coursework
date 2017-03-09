package uk.ac.gla.dcs.dsms;

import org.terrier.structures.postings.BlockPosting;
import org.terrier.structures.postings.IterablePosting;
import org.terrier.matching.dsms.DependenceScoreModifier;

/**
 *
 */
public class MinDistProximityFeatureDSM extends DependenceScoreModifier {

    @Override
    protected double calculateDependence(IterablePosting[] ips, boolean[] okToUse,
                                         double[] phraseTermWeights, boolean SD) {

        int numberOfPairs = 0;


        double score = 0.0d;
        for (int i=0; i< ips.length -1 ; i++) {
            if (okToUse[i]) {
                for (int j = i+1; j< ips.length ; j++) {
                    if (okToUse[j]) {
                        numberOfPairs +=1;
                        BlockPosting firstBP = (BlockPosting) ips[i];
                        int[]firstPosList = firstBP.getPositions();
                        BlockPosting secondBP = (BlockPosting) ips[j];
                        int[]secondPosList = secondBP.getPositions();
                        score += getMinDistBetweenPostingLists(firstPosList, secondPosList);
                    }
                }
            }
        }

        return (numberOfPairs == 0) ? 0.0d : score/numberOfPairs;
    }

    private double getMinDistBetweenPostingLists(int[]firstPosList, int[]secondPosList) {
        double min = Double.MAX_VALUE;

        for (int k=0; k< firstPosList.length;  k++) {
            for (int l=0; l< secondPosList.length; l++) {
                double dist = Math.abs(secondPosList[l]-firstPosList[k]);
                if (min > dist) {
                    min = dist;
                }
            }
        }

        return min;
    }

    @Override
    protected double scoreFDSD(int matchingNGrams, int docLength) {
        throw new UnsupportedOperationException();
    }


    @Override
    public String getName() {
        return "Min_Dist_Proximity_Feature_DSM";
    }

}
