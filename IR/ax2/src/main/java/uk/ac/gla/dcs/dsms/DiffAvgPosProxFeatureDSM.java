package uk.ac.gla.dcs.dsms;

import org.terrier.structures.postings.BlockPosting;
import org.terrier.structures.postings.IterablePosting;

import java.util.ArrayList;
import java.util.List;

import org.terrier.matching.dsms.DependenceScoreModifier;

/** 
 * This class is implementing the diff_avg_pos(a, b, D) function from proximity measures section 
 * <p> It calculates the distance between the average positions of query terms a and b in document D.
 * The class first calculates the average position of each query term and then it computes the distance 
 * between every single pair of average positions.
 * The mean value is calculated from those distances and set as the score of the document with respect to the query. 
 * @author 2082041s
 */
public class DiffAvgPosProxFeatureDSM extends DependenceScoreModifier {


	/** This class is passed the postings of the current document,
	 * and should return a score to represent that document.
	 */
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
		
		// calculate average position list for each ips
		List<Double> avgPosList = new ArrayList<>();
		for (int i=0; i< ips.length ; i++)
		{
			if (okToUse[i])
			{
				BlockPosting bp = (BlockPosting) ips[i]; 
				int[]posList = bp.getPositions();
				double avg_pos = 0;
				for (int pos : posList)
				{
					avg_pos += pos;
				}
				avg_pos /= posList.length;
				avgPosList.add(avg_pos);
			}
		}
		
		for (int i=0; i< avgPosList.size()-1;  i++)
			for (int j=i+1; j< avgPosList.size(); j++)
			{
				numberOfPairs +=1;
				score += Math.abs(avgPosList.get(j)-avgPosList.get(i));
			}
		
		if (numberOfPairs == 0)
			return 0.0d;
		
		score /= numberOfPairs;	
		return score;
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
