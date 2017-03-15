import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.hbase.HBaseConfiguration;
import org.apache.hadoop.hbase.client.Scan;
import org.apache.hadoop.hbase.io.ImmutableBytesWritable;
import org.apache.hadoop.hbase.mapreduce.TableMapReduceUtil;
import org.apache.hadoop.hbase.util.Bytes;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

public class MainTaskTwo extends Configured implements Tool {

    public static final String outputPath = "hdfs:///user/2147392n/ax2_task2";
	public static final String inputPath = "BD4:enwiki-perftest";

	@Override
	public int run(String[] args) throws Exception {
		Configuration conf = HBaseConfiguration.create(getConf());
		conf.addResource(new Path("/local/bd4/bd4-hadoop-ug/conf/core-site.xml"));
		conf.set("mapred.jar", "ax2_task2.jar");
		conf.set("args", args[0]);
		Job job = Job.getInstance(conf);
		job.setJarByClass(MainTaskTwo.class);
		job.setJobName("BD4-AX2-TASK2-2147392n");
		Scan scan = new Scan();
		scan.setCaching(100);
		scan.setCacheBlocks(false);
		scan.addFamily(Bytes.toBytes("WD"));
		TableMapReduceUtil.initTableMapperJob(inputPath, scan, BMapper.class,
                LongWritable.class, RevisionTimestampPair.class, job);
		job.setOutputFormatClass(TextOutputFormat.class);
		job.setReducerClass(BReducer.class);
		FileOutputFormat.setOutputPath(job, new Path(outputPath));
		job.setNumReduceTasks(1);
		job.submit();
		return job.waitForCompletion(true) ? 0 : 1;
	}

	public static void main(String[] args) throws Exception {
		System.exit(ToolRunner.run(new MainTaskTwo(), args));
	}
}
