package task2;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.hbase.HBaseConfiguration;
import org.apache.hadoop.hbase.client.Scan;
import org.apache.hadoop.hbase.mapreduce.TableMapReduceUtil;
import org.apache.hadoop.hbase.util.Bytes;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

public class MainTaskTwo extends Configured implements Tool {
	@Override
	public int run(String[] args) throws Exception {
		Configuration conf = HBaseConfiguration.create(getConf());
		conf.addResource(new Path("/local/bd4/bd4-hadoop-ug/conf/core-site.xml"));
		conf.set("mapred.jar", "ax2_task2.jar");
		conf.setStrings("args", args[0], args[1], args[2]);
		Job job = Job.getInstance(conf);
		job.setJarByClass(MainTaskTwo.class);
		job.setJobName("BD4-AX2-TASK2-2147392n");
		Scan scan = new Scan();
		scan.setCaching(100);
		scan.setCacheBlocks(false);
		scan.addFamily(Bytes.toBytes("WD"));
		TableMapReduceUtil.initTableMapperJob(args[0], scan, MapperTaskTwo.class,
                LongWritable.class, UtilityPairRevisionTimestamp.class, job);
		job.setOutputFormatClass(TextOutputFormat.class);
		job.setReducerClass(ReducerTaskTwo.class);
		FileOutputFormat.setOutputPath(job, new Path(args[1]));
		job.setNumReduceTasks(1);
		job.submit();
		return job.waitForCompletion(true) ? 0 : 1;
	}

	public static void main(String[] args) throws Exception {
		System.exit(ToolRunner.run(new MainTaskTwo(), args));
	}
}
