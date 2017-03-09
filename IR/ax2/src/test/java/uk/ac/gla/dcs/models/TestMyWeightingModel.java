package uk.ac.gla.dcs.models;

import org.junit.Test;
import static org.junit.Assert.*;
import org.terrier.indexing.IndexTestUtils;
import org.terrier.matching.models.WeightingModel;
import org.terrier.structures.Index;
import org.terrier.tests.ApplicationSetupBasedTest;
import org.terrier.utility.ApplicationSetup;

public class TestMyWeightingModel extends ApplicationSetupBasedTest
{
	@Test public void testModel() throws Exception
	{
		//no stemming or stopwords
		ApplicationSetup.setProperty("termpipelines", "");
		//make an index with a two sample documents		
		Index index = IndexTestUtils.makeIndex(
				new String[]{"doc1", "doc2"}, 
				new String[]{"The quick brown fox jumps over the lazy dog", 
					"Exploring the zoo, we saw every kangaroo jump and quite a few carried babies."});
		
		WeightingModel wm = new MyWeightingModel();
		wm.setCollectionStatistics(index.getCollectionStatistics());
		//double check: our index has two documents
		assertEquals(2, index.getCollectionStatistics().getNumberOfDocuments());
		
		//what score would your weighting model give to the term 'jumps', 
		// if it occurred once in a document with total length 5?
		wm.setEntryStatistics(index.getLexicon().getLexiconEntry("jumps"));
		wm.setKeyFrequency(1.0d);
		wm.prepare();
		double score = wm.score(1, 5);
		System.out.println(score);
		//TODO: make your assertion about what the score should be
		//assertEquals(XXX, score, 0.0d);
	}
}
