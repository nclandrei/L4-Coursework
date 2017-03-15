package task1;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;

public class ReducerTaskOne extends Reducer<LongWritable, LongWritable, LongWritable, Text> {
	public void reduce(LongWritable key,Iterable<LongWritable> values, Context context)
            throws IOException,InterruptedException {
        Text _value = new Text();
		List<Long> revisionsList = new ArrayList<>();

		for (LongWritable value : values) {
			revisionsList.add(value.get());
		}
		
		Collections.sort(revisionsList);

		StringBuilder sb = new StringBuilder();
		sb.append(revisionsList.size());

		for(Long rev: revisionsList){
			sb.append(" ").append(rev);
		}

		_value.set(sb.toString());
		context.write(key, _value);
	}
}
