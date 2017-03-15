package task2;

import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class ReducerTaskTwo extends Reducer<LongWritable, RevisionTimestampPair, LongWritable, Text> {
	public void reduce(LongWritable key,Iterable<RevisionTimestampPair> values, Context context) throws IOException,InterruptedException {
		Text _value = new Text();
		
		long mostRecentTimestamp = 0;
		long mostRecentRevisionID = 0;

        for (RevisionTimestampPair rev : values) {
            long timestamp = rev.getTimestamp().get();

            if (timestamp > mostRecentTimestamp) {
                mostRecentTimestamp = timestamp;
                mostRecentRevisionID = rev.getRevisionID().get();
            }
        }
		
		StringBuilder sb = new StringBuilder();
		sb.append(mostRecentRevisionID).append(" ");
		
		Date timestampDate = new Date(mostRecentTimestamp);
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
		sb.append(df.format(timestampDate));
	
		_value.set(sb.toString());
		context.write(key, _value);
	}
}
