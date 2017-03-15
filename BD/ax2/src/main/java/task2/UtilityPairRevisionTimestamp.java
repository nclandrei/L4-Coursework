package task2;

import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.io.WritableComparable;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

public class UtilityPairRevisionTimestamp implements Writable, WritableComparable<UtilityPairRevisionTimestamp > {

    private LongWritable timestamp;
	private LongWritable revision;

	UtilityPairRevisionTimestamp(long revisionID, long timestamp) {
		this.revision = new LongWritable(revisionID);
		this.timestamp = new LongWritable(timestamp);
	}

	@Override
	public void readFields(DataInput in) throws IOException {
		revision = new LongWritable(in.readLong());
		timestamp = new LongWritable(in.readLong());
	}

	@Override
	public void write(DataOutput out) throws IOException {
		out.writeLong(timestamp.get());
        out.writeLong(revision.get());
	}

	LongWritable getRevisionID() {
		return revision;
	}

	LongWritable getTimestamp() {
		return timestamp;
	}

    @Override
    public int compareTo(UtilityPairRevisionTimestamp o) {
        return 0;
    }
}
