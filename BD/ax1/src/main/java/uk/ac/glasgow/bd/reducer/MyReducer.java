package main.java.uk.ac.glasgow.bd.reducer;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;

import java.io.IOException;
import java.util.Iterator;

public class MyReducer extends Reducer<Text, IntWritable, Text, IntWritable> {
    private IntWritable _value = new IntWritable(); protected void reduce(Text key, Iterable<IntWritable> values, Context
            context) throws IOException, InterruptedException { int sum = 0;
        for (Iterator<IntWritable> it = values.iterator(); it.hasNext();)
            sum += it.next().get();
        _value.set(sum);
        context.write(key, _value);
    }
}
