package task1;

import java.io.IOException;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.io.ImmutableBytesWritable;
import org.apache.hadoop.hbase.mapreduce.TableMapper;
import org.apache.hadoop.hbase.util.Bytes;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;

public class MapperTaskOne extends TableMapper<LongWritable, LongWritable> {
    private long startTimestamp, endTimestamp;
	private LongWritable _key = new LongWritable();
	private LongWritable _value = new LongWritable();

	protected void setup(Context context) {
		Configuration conf = context.getConfiguration();
		String[] args = conf.getStrings("args");
		startTimestamp = javax.xml.bind.DatatypeConverter.parseDateTime(args[0]).getTime().getTime();
		endTimestamp = javax.xml.bind.DatatypeConverter.parseDateTime(args[1]).getTime().getTime();
	}
	
	public void map(ImmutableBytesWritable key, Result value, Context context)
            throws IOException, InterruptedException {
		long timestamp = value.rawCells()[0].getTimestamp();
        long article = Bytes.toLong(key.get(), 0);
        long revision = Bytes.toLong(key.get(), 8);

		if (startTimestamp <= timestamp && endTimestamp >= timestamp) {
			_key.set(article);
			_value.set(revision);
			context.write(_key, _value);
		}
	}
}
