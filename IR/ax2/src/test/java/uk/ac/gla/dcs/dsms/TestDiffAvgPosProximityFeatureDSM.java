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

/**
 * Test class for the diff_avg_pos proximity feature DSM
 * @author 2147392n
 */
public class TestDiffAvgPosProximityFeatureDSM extends ApplicationSetupBasedTest {
	@Test
    public void testOneDocTwoTerms() throws Exception {
		ApplicationSetup.setProperty("termpipelines", "");
		Index index = IndexTestUtils.makeIndexBlocks(
				new String[]{"doc1"}, 
				new String[]{"The lazy brown fox jumps over the lazy dog"});

		IterablePosting[] ips = new IterablePosting[3];

		ips[0] = index.getInvertedIndex().getPostings(index.getLexicon().getLexiconEntry("fox"));
		ips[1] = index.getInvertedIndex().getPostings(index.getLexicon().getLexiconEntry("jumps"));
        ips[2] = index.getInvertedIndex().getPostings(index.getLexicon().getLexiconEntry("lazy"));

		ips[0].next();
		ips[1].next();
        ips[2].next();

		assertEquals(0, ips[0].getId());
		assertEquals(0, ips[1].getId());
        assertEquals(0, ips[2].getId());

		System.out.println("Positions of term 'fox'="+ Arrays.toString( ((BlockPosting)ips[0]).getPositions()));
		System.out.println("Positions of term 'jumps'="+ Arrays.toString( ((BlockPosting)ips[1]).getPositions()));
        System.out.println("Positions of term 'lazy'="+ Arrays.toString( ((BlockPosting)ips[2]).getPositions()));

		DiffAvgPosProximityFeatureDSM diffAvgPosProximityFeatureDSM = new DiffAvgPosProximityFeatureDSM();

        IterablePosting[] testSingleIPList = new IterablePosting[1];
        testSingleIPList[0] = ips[0];

        double score = diffAvgPosProximityFeatureDSM.calculateDependence(testSingleIPList, new boolean[]{true},
                new double[]{1d}, false);
        assertEquals(0.0d, score, 0.0d);

        score = diffAvgPosProximityFeatureDSM.calculateDependence(ips, new boolean[]{true, true, true},
                new double[]{1d, 1d, 1d}, false);
		assertEquals(2/3.0, score, 0.0d);
	}
}
