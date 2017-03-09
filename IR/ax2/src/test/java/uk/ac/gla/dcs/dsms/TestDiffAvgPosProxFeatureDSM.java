package uk.ac.gla.dcs.dsms;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Test;
import org.terrier.indexing.IndexTestUtils;
import org.terrier.structures.Index;
import org.terrier.structures.postings.BlockPosting;
import org.terrier.structures.postings.IterablePosting;
import org.terrier.tests.ApplicationSetupBasedTest;
import org.terrier.utility.ApplicationSetup;

public class TestDiffAvgPosProxFeatureDSM extends ApplicationSetupBasedTest
{
	@Test public void testOneDocTwoTerms() throws Exception {

		//make an index with a single sample document
		ApplicationSetup.setProperty("termpipelines", "");
		Index index = IndexTestUtils.makeIndexBlocks(
				new String[]{"doc1"}, 
				new String[]{"The quick brown fox jumps over the quick dog"});

		//get posting iterators for two terms 'fox' and 'jumps'
		IterablePosting[] ips = new IterablePosting[3];
		System.out.println(index.getLexicon().getLexiconEntry("fox"));
		System.out.println(index.getLexicon().getLexiconEntry("jumps"));
		ips[0] = index.getInvertedIndex().getPostings(index.getLexicon().getLexiconEntry("fox"));
		ips[1] = index.getInvertedIndex().getPostings(index.getLexicon().getLexiconEntry("jumps"));
		ips[2] = index.getInvertedIndex().getPostings(index.getLexicon().getLexiconEntry("quick"));
		ips[0].next();
		ips[1].next();
		ips[2].next();
		assertEquals(0, ips[0].getId());
		assertEquals(0, ips[1].getId());
		assertEquals(0, ips[2].getId());
		System.out.println("Positions of term 'fox'="+ Arrays.toString( ((BlockPosting)ips[0]).getPositions()));
		System.out.println("Positions of term 'jumps'="+ Arrays.toString( ((BlockPosting)ips[1]).getPositions()));
		System.out.println("Positions of term 'quick'="+ Arrays.toString( ((BlockPosting)ips[2]).getPositions()));

		DiffAvgPosProxFeatureDSM sample = new DiffAvgPosProxFeatureDSM();
		double score = sample.calculateDependence(
            ips, //posting lists
            new boolean[]{true,true,true},  //is this posting list on the correct document?
            new double[]{1d,1d,1d}, false//doesnt matter
		);
		System.out.println(score);
		// average position of quick = (1+7)/2 = 4
		// average position of fox = 3
		// average position of dog = 4
		// score = (abs(4-3) + abs(4-4) + abs(3-4))/3 = 2/3
		assertEquals(2/3.0, score, 0.0d);
		
		IterablePosting[] singleIps = new IterablePosting[1];
		singleIps[0] = ips[0];
		score = sample.calculateDependence(
			singleIps, //posting lists
            new boolean[]{true},  //is this posting list on the correct document?
            new double[]{1d}, false//doesnt matter
		);
		
		// return 0 in case query has only 1 term 
		assertEquals(0.0d, score, 0.0d);
	}
}
