package main.java.uk.ac.glasgow.bd;

import main.java.uk.ac.glasgow.bd.format.MyInputFormat;
import main.java.uk.ac.glasgow.bd.mapper.LineMapper;
import main.java.uk.ac.glasgow.bd.partitioner.MyPartitioner;
import main.java.uk.ac.glasgow.bd.reducer.MyReducer;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;


public class Driver extends Configured implements Tool {

    public int run(String[] args) throws Exception {
        Configuration conf = new Configuration();
        conf.addResource(new Path("/local/bd4/bd4-hadoop-ug/conf/core-site.xml"));
        conf.set("startDate", args[0]);
        conf.set("endDate", args[1]);
        conf.set("k", args[2]);
        Job job = Job.getInstance(conf);
        job.setJobName("MyWordCount(" + args[0] + ")");
        job.setJarByClass(Driver.class);
        job.setInputFormatClass(MyInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);
        job.setMapperClass(LineMapper.class);
        job.setPartitionerClass(MyPartitioner.class);
        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(IntWritable.class);
        job.setReducerClass(MyReducer.class);
        job.setCombinerClass(MyReducer.class);
        job.setNumReduceTasks(1);
        FileInputFormat.setInputPaths(job, "/user/bd4-ae1/enwiki-20080103-perftest.txt");
        FileOutputFormat.setOutputPath(job, new Path("user/2147392n/bd4-output"));
        job.submit();
        return job.waitForCompletion(true) ? 0 : 1;
    }

    public static void main(String[] args) throws Exception {
        System.exit(ToolRunner.run(new Driver(), args));
    }

}
