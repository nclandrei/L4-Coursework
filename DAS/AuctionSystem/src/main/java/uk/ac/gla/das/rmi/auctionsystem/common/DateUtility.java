package uk.ac.gla.das.rmi.auctionsystem.common;

import java.text.SimpleDateFormat;

public class DateUtility {
    public final static String PATTERN = "dd-MM-yyy'T'HH:mm";

    public static SimpleDateFormat getDateFormat () {
        return new SimpleDateFormat(PATTERN);
    }
}
