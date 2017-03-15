package task2;

import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.io.WritableComparable;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

public class RevisionTimestampPair implements Writable, WritableComparable<RevisionTimestampPair> {
	
	private LongWritable revisionID;
	private LongWritable timestamp;
	
	RevisionTimestampPair(long revisionID, long timestamp) {
		this.revisionID = new LongWritable(revisionID);
		this.timestamp = new LongWritable(timestamp);
	}

	@Override
	public void readFields(DataInput in) throws IOException {
		revisionID = new LongWritable(in.readLong());
		timestamp = new LongWritable(in.readLong());
	}

	@Override
	public void write(DataOutput out) throws IOException {
		out.writeLong(revisionID.get());
		out.writeLong(timestamp.get());
		
	}

	LongWritable getRevisionID() {
		return revisionID;
	}

	LongWritable getTimestamp() {
		return timestamp;
	}

    @Override
    public int compareTo(RevisionTimestampPair o) {
        return 0;
    }
}
