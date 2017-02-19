package main.java.uk.ac.glasgow.bd.reader;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.DataOutputBuffer;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapred.FileSplit;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.TaskAttemptContext;

import java.io.IOException;

public class MyRecordReader extends RecordReader<LongWritable, Text> {
    private static final byte[] recordSeparator = "\t\t\t".getBytes();
    private FSDataInputStream fsin;
    private long start, end;
    private boolean stillInChunk = true;
    private DataOutputBuffer buffer = new DataOutputBuffer();
    private LongWritable key = new LongWritable();
    private Text value = new Text();

    @Override
    public void initialize(InputSplit inputSplit, TaskAttemptContext context) throws IOException {
        FileSplit split = (FileSplit) inputSplit;
        Configuration conf = context.getConfiguration();
        Path path = split.getPath();
        FileSystem fs = path.getFileSystem(conf);

        fsin = fs.open(path);
        start = split.getStart();
        end = split.getStart() + split.getLength();
        fsin.seek(start);

        if (start != 0)
            readRecord(false);
    }

    private boolean readRecord(boolean withinBlock) throws IOException {
        int i = 0, b;
        while (true) {
            if ((b = fsin.read()) == -1)
                return false;
            if (withinBlock) {
                buffer.write(b);
            }
            if (b == recordSeparator[i]) {
                if (++i == recordSeparator.length) {
                    return fsin.getPos() < end;
                }
            }
            else {
                i = 0;
            }
        }
    }

    @Override
    public boolean nextKeyValue() throws IOException, InterruptedException {
        if (!stillInChunk) {
            return false;
        }
        boolean status = readRecord(true);
        value = new Text();
        value.set(buffer.getData(), 0, buffer.getLength());
        key.set(fsin.getPos());
        buffer.reset();
        if (!status)
            stillInChunk = false;
        return true;
    }

    @Override
    public LongWritable getCurrentKey() throws IOException, InterruptedException {
        return key;
    }

    @Override
    public Text getCurrentValue() throws IOException, InterruptedException {
        return value;
    }

    @Override
    public float getProgress() throws IOException, InterruptedException {
        return (float) (fsin.getPos() - start) / (end - start);
    }

    @Override
    public void close() throws IOException {
        fsin.close();
    }
}
