package task2;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.io.ImmutableBytesWritable;
import org.apache.hadoop.hbase.mapreduce.TableMapper;
import org.apache.hadoop.hbase.util.Bytes;
import org.apache.hadoop.io.LongWritable;

import java.io.IOException;

public class MapperTaskTwo extends TableMapper<LongWritable, UtilityPairRevisionTimestamp> {
	private LongWritable _key = new LongWritable();
	private long currentTimestamp;
	
	protected void setup(Context context) {
		Configuration conf = context.getConfiguration();
		String timestamp = conf.get("args");
		this.currentTimestamp = javax.xml.bind.DatatypeConverter.parseDateTime(timestamp).getTime().getTime();
	}
	
	public void map(ImmutableBytesWritable key, Result value, Context context) throws IOException, InterruptedException {
        long revision = Bytes.toLong(key.get(), 8);
        long timestamp = value.rawCells()[0].getTimestamp();
		long article = Bytes.toLong(key.get(), 0);

		if (timestamp <= currentTimestamp) {
			_key.set(article);
			UtilityPairRevisionTimestamp pairVal = new UtilityPairRevisionTimestamp(revision, timestamp);
			context.write(_key, pairVal);
		}
	}
}
