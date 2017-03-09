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

public class TestDiffAvgPosProximityFeatureDSM extends ApplicationSetupBasedTest {
	@Test
    public void testOneDocTwoTerms() throws Exception {
		ApplicationSetup.setProperty("termpipelines", "");
		Index index = IndexTestUtils.makeIndexBlocks(
				new String[]{"doc1"}, 
				new String[]{"The quick brown fox jumps over the lazy dog"});

		IterablePosting[] ips = new IterablePosting[2];
		ips[0] = index.getInvertedIndex().getPostings(index.getLexicon().getLexiconEntry("fox"));
		ips[1] = index.getInvertedIndex().getPostings(index.getLexicon().getLexiconEntry("jumps"));
		ips[0].next();
		ips[1].next();
		assertEquals(0, ips[0].getId());
		assertEquals(0, ips[1].getId());
		System.out.println("Positions of term 'fox'="+ Arrays.toString( ((BlockPosting)ips[0]).getPositions()));
		System.out.println("Positions of term 'jumps'="+ Arrays.toString( ((BlockPosting)ips[1]).getPositions()));

		DiffAvgPosProximityFeatureDSM diffAvgPosProximityFeatureDSM = new DiffAvgPosProximityFeatureDSM();

        IterablePosting[] testIps = new IterablePosting[1];
        testIps[0] = ips[0];

        double score = diffAvgPosProximityFeatureDSM.calculateDependence(testIps, new boolean[]{true},
                new double[]{1d}, false);
        assertEquals(0.0d, score, 0.0d);

        score = diffAvgPosProximityFeatureDSM.calculateDependence(ips, new boolean[]{true,true},
                new double[]{1d,1d}, false);
		System.out.println(score);

		assertEquals(2/3.0, score, 0.0d);


	}
}
