package uk.ac.gla.dcs.dsms;

import org.terrier.structures.postings.BlockPosting;
import org.terrier.structures.postings.IterablePosting;

import org.terrier.matching.dsms.DependenceScoreModifier;

public class MinDistProxFeatureDSM extends DependenceScoreModifier {

	@Override
	protected double calculateDependence(
			IterablePosting[] ips, //posting lists
			boolean[] okToUse,  //is this posting list on the correct document?
			double[] phraseTermWeights, boolean SD //not needed
		) 
	{
		
		final int numberOfQueryTerms = okToUse.length;
		int numberOfPairs = 0;


		double score = 0.0d;  
		for (int i=0; i< ips.length -1 ; i++)
		{
			if (okToUse[i])
			{
				for (int j = i+1; j< ips.length ; j++)
				{
					if (okToUse[j])
					{
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
		
		// if there are no pairs i.e. only 1 query term then return 0
		if (numberOfPairs == 0)
		{
			return 0.0d;
		}
		score /= numberOfPairs;	
		return score;
	}

	private double getMinDistBetweenPostingLists(int[]firstPosList, int[]secondPosList) {
		double min = Double.MAX_VALUE;
		
		for (int k=0; k< firstPosList.length;  k++)
		{
			
			for (int l=0; l< secondPosList.length; l++)
			{
				double dist = Math.abs(secondPosList[l]-firstPosList[k]);
				if (min > dist)
				{
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
		return "ProxFeatureDSM_MYFUNCTION";
	}
	
}
