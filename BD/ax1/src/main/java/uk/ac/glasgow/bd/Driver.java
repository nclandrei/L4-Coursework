package main.java.uk.ac.glasgow.bd;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.FileInputFormat;
import org.apache.hadoop.mapred.FileOutputFormat;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;


public class Driver extends Configured implements Tool {

    public int run(String[] args) throws Exception {
        Job job = new Job();
        job.setJobName("MyWordCount(" + args[0] + ")");
        job.setJarByClass(Driver.class);
        job.setInputFormatClass(MyInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);
        job.setMapperClass(MyMapper.class);
        job.setPartitionerClass(MyPartitioner.class);
        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(IntWritable.class);
        job.setReducerClass(MyReducer.class);
        job.setCombinerClass(MyReducer.class);
        FileInputFormat.setInputPaths(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(job.getJobName() +
                "_output"));
        job.submit();
        return job.waitForCompletion(true) ? 0 : 1;
    }

    public static void main(String[] args) throws Exception {
        System.exit(ToolRunner.run(new Driver(), args));
    }

}
