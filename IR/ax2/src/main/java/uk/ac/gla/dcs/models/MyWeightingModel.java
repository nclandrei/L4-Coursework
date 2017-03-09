package uk.ac.gla.dcs.models;

import org.terrier.matching.models.WeightingModel;

public class MyWeightingModel extends WeightingModel {
	public String getInfo() {
		return this.getClass().getSimpleName();
	}
	
	public double score(double tf, double docLength) {
		return 0d;
	}

	public double score(double tf, double docLength, double n_t, double F_t, double _keyFrequency) {
		throw new UnsupportedOperationException();
	}
}
