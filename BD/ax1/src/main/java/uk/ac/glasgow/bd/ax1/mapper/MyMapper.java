package main.java.uk.ac.glasgow.bd.ax1.mapper;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class MyMapper extends Mapper<LongWritable, Text, IntWritable, IntWritable> {

    private IntWritable _key = new IntWritable();
    private IntWritable _value = new IntWritable();

    protected void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
        String[] currentRecord = value.toString().split(" ");
        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ss'Z'");

        try {
            Date endDate =  dateFormat.parse(context.getConfiguration().get("endDate"));
            Date startDate =  dateFormat.parse(context.getConfiguration().get("startDate"));
            if (dateFormat.parse(currentRecord[4]).compareTo(startDate) > 0 && dateFormat.parse(currentRecord[4]).compareTo(endDate) < 0) {
                _key.set(Integer.parseInt(currentRecord[1]));
                _value.set(1);
                context.write(_key,_value);
            }

        }
        catch (ParseException ex) {
            System.err.println("Could not parse the dates.");
            ex.printStackTrace();
        }
    }

}
