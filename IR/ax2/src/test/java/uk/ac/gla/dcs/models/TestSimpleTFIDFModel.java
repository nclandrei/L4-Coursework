package uk.ac.gla.dcs.models;

import org.junit.Test;
import static org.junit.Assert.*;

import org.terrier.indexing.IndexTestUtils;
import org.terrier.matching.models.WeightingModel;
import org.terrier.structures.Index;
import org.terrier.tests.ApplicationSetupBasedTest;
import org.terrier.utility.ApplicationSetup;

public class TestSimpleTFIDFModel extends ApplicationSetupBasedTest {

	@Test
	public void testModel() throws Exception {
		ApplicationSetup.setProperty("termpipelines", "");
		Index index = IndexTestUtils.makeIndex(
				new String[]{"doc1", "doc2"}, 
				new String[]{"The quick brown fox jumps over the lazy dog", 
					"Exploring the zoo, we saw every kangaroo jump and quite a few carried babies."});
		
		WeightingModel wm = new SimpleTFIDFModel();
		wm.setCollectionStatistics(index.getCollectionStatistics());
		assertEquals(2, index.getCollectionStatistics().getNumberOfDocuments());
		
		wm.setEntryStatistics(index.getLexicon().getLexiconEntry("jumps"));
		wm.setKeyFrequency(1.0d);
		wm.prepare();

		double score = wm.score(1, 5);
		assertEquals(0.0d, score, 0.0d);
		
		score = wm.score(1, 100);
		assertEquals(0.0d, score, 0.0d);
		
		wm.setKeyFrequency(0.0d);
		score = wm.score(0, 100); 
		assertEquals(0.0d, score, 0.0d);
	}
}
