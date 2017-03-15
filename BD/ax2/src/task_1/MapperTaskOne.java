import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.io.ImmutableBytesWritable;
import org.apache.hadoop.hbase.mapreduce.TableMapper;
import org.apache.hadoop.hbase.util.Bytes;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;

public class MapperTaskOne extends TableMapper<LongWritable, LongWritable> {
	private LongWritable _key = new LongWritable();
	private LongWritable _value = new LongWritable();
	long startTimestamp, endTimestamp;
	
	protected void setup(Context context) {
		Configuration conf = context.getConfiguration();
		String[] arguments = conf.getStrings("arguments");
		String start = arguments[0];
		String end = arguments[1];
		startTimestamp = javax.xml.bind.DatatypeConverter.parseDateTime(start).getTime().getTime();
		endTimestamp = javax.xml.bind.DatatypeConverter.parseDateTime(end).getTime().getTime();
	}
	
	public void map(ImmutableBytesWritable key, Result value, Context context) throws IOException, InterruptedException {
		long articleID = Bytes.toLong(key.get(), 0);
		long revisionID = Bytes.toLong(key.get(), 8);
		long timestamp = value.rawCells()[0].getTimestamp();
		
		if (startTimestamp <= timestamp && endTimestamp >= timestamp) {
			_key.set(articleID);
			_value.set(revisionID);
			context.write(_key, _value);
		}
	}
}
