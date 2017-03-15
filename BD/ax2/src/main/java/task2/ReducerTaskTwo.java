package task2;

import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class ReducerTaskTwo extends Reducer<LongWritable, UtilityPairRevisionTimestamp, LongWritable, Text> {
	public void reduce(LongWritable key,Iterable<UtilityPairRevisionTimestamp> values, Context context)
            throws IOException,InterruptedException {
		Text _value = new Text();
        long resultRevision = 0;
		long resultTimestamp = 0;

        for (UtilityPairRevisionTimestamp rev : values) {
            long timestamp = rev.getTimestamp().get();
            if (timestamp > resultTimestamp) {
                resultTimestamp = timestamp;
                resultRevision = rev.getRevisionID().get();
            }
        }
		
		StringBuilder sb = new StringBuilder();
		sb.append(resultRevision).append(" ");
		
		Date timestampDate = new Date(resultTimestamp);
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
		sb.append(df.format(timestampDate));
	
		_value.set(sb.toString());
		context.write(key, _value);
	}
}
