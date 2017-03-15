import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;

public class ReducerTaskOne extends Reducer<LongWritable, LongWritable, LongWritable, Text> {
	public void reduce(LongWritable key,Iterable<LongWritable> values, Context context) throws IOException,InterruptedException {
		List<Long> revisions = new ArrayList<Long>();
		Text _value = new Text();
	
		for (Iterator<LongWritable> it = values.iterator(); it.hasNext();) {
			revisions.add(it.next().get());
		}
		
		Collections.sort(revisions);
		StringBuilder sb = new StringBuilder();
		sb.append(revisions.size());
		for(Long rev: revisions){
			sb.append(" " + rev);
		}
		_value.set(sb.toString());
		context.write(key, _value);
	}
}
