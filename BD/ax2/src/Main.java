import main.java.uk.ac.glasgow.bd.ax2.reducer.CustomReducer;
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

import static com.oracle.jrockit.jfr.ContentType.Bytes;

public class Main extends Configured implements Tool {

	public static final String inputFile = "BD4:enwiki-perftest";
	public static final String outputFile = "hdfs:///user/2088959m/ax2";
	
	@Override
	public int run(String[] args) throws Exception {
		Configuration conf = HBaseConfiguration.create(getConf());
		conf.addResource(new Path("/local/bd4/bd4-hadoop-ug/conf/core-site.xml"));
		conf.set("mapred.jar", "file:///users/level4/2088959m/workspace/bd/bd4ae2.jar");
		conf.setStrings("arguments", args[0], args[1]);
		
		Job job = Job.getInstance(conf);
		job.setJarByClass(Main.class);
		
		
		Scan scan = new Scan();
		scan.setCaching(100);
		scan.setCacheBlocks(false);
		scan.addFamily(Bytes.toBytes("WD"));
		
		TableMapReduceUtil.initTableMapperJob(inputFile, scan, CustomMapper.class, LongWritable.class, LongWritable.class, job);
		
		job.setOutputFormatClass(TextOutputFormat.class);
		job.setReducerClass(CustomReducer.class);
		
		FileOutputFormat.setOutputPath(job, new Path(outputFile));
		
		job.setNumReduceTasks(1);
		
		job.submit();
		
		return job.waitForCompletion(true) ? 0 : 1;
		
	}

	public static void main(String[] args) throws Exception {
		System.exit(ToolRunner.run(new Main(), args));
	}

}
