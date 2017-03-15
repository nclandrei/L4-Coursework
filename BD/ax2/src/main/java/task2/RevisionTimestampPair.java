import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.io.WritableComparable;
import org.apache.hadoop.io.WritableUtils;

public class RevisionTimestampPair implements Writable, WritableComparable<RevisionTimestampPair> {
	
	private LongWritable revisionID = new LongWritable();
	private LongWritable timestamp = new LongWritable();
	
	public RevisionTimestampPair() {}
	
	public RevisionTimestampPair(long revisionID, long timestamp) {
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
	
	@Override
	public String toString() {
		return "RevisionID: " + revisionID.toString() + " - Timestamp: " + timestamp.toString();
	}


	public LongWritable getRevisionID() {
		return revisionID;
	}

	public void setRevisionID(LongWritable revisionID) {
		this.revisionID = revisionID;
	}

	public LongWritable getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(LongWritable timestamp) {
		this.timestamp = timestamp;
	}

	@Override
	public int compareTo(RevisionTimestampPair o) {
		// TODO Auto-generated method stub
		return 0;
	}

	

	

}
