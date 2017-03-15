package task2;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.io.ImmutableBytesWritable;
import org.apache.hadoop.hbase.mapreduce.TableMapper;
import org.apache.hadoop.hbase.util.Bytes;
import org.apache.hadoop.io.LongWritable;

import java.io.IOException;

public class MapperTaskTwo extends TableMapper<LongWritable, RevisionTimestampPair> {
	private LongWritable _key = new LongWritable();
	long currentTimestamp;
	
	protected void setup(Context context) {
		Configuration conf = context.getConfiguration();
		String timestamp = conf.get("args");
		currentTimestamp = javax.xml.bind.DatatypeConverter.parseDateTime(timestamp).getTime().getTime();
	}
	
	public void map(ImmutableBytesWritable key, Result value, Context context) throws IOException, InterruptedException {
		long articleID = Bytes.toLong(key.get(), 0);
		long revisionID = Bytes.toLong(key.get(), 8);
		long timestamp = value.rawCells()[0].getTimestamp();
		
		if (timestamp <= currentTimestamp) {
			_key.set(articleID);
			RevisionTimestampPair compositeValue = new RevisionTimestampPair(revisionID, timestamp);
			context.write(_key, compositeValue);
		}
	}
}
