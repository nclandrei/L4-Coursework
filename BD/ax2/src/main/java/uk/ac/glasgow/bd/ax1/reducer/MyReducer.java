package main.java.uk.ac.glasgow.bd.ax1.reducer;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.mapreduce.Reducer;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class MyReducer extends Reducer<IntWritable, IntWritable, IntWritable, IntWritable> {
    private IntWritable _value = new IntWritable();
    private IntWritable _key = new IntWritable();

    private Map<Integer, Integer> map;
    private Integer k;

    @Override
    protected void setup(Context context) throws IOException, InterruptedException {
        Configuration conf = context.getConfiguration();
        map = new HashMap<>();
        k = Integer.parseInt(conf.get("k"));
    }

    @Override
    protected void reduce(IntWritable key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException {
        int sum = 0;
        for (IntWritable value : values) {
            sum += value.get();
        }
        map.put(key.get(), sum);
    }

    @Override
    protected void cleanup(Context context) throws IOException, InterruptedException {
        map.entrySet().stream()
                .sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
                .limit(k)
                .forEach(x -> {
                    try {
                        _key.set(x.getKey());
                        _value.set(x.getValue());
                        context.write(_key, _value);
                    } catch (IOException | InterruptedException e) {
                        e.printStackTrace();
                    }
                });
    }
}
