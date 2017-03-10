package uk.ac.gla.dcs.models;

import org.terrier.matching.models.WeightingModel;

public class Andrei_Mihai_Nicolae_SimpleTFIDFModel extends WeightingModel {

    public String getInfo() {
        return this.getClass().getSimpleName();
    }

    public double score(double tf, double docLength) {
        double denominator = (numberOfDocuments -documentFrequency) + 0.5;
        double nominator = documentFrequency + 0.5;
        return tf * Math.log(denominator/nominator);
    }

    public double score(double tf, double docLength, double n_t, double F_t, double _keyFrequency) {
        throw new UnsupportedOperationException();
    }
}
