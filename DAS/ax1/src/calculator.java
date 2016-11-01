/*
	Code: Calculator Interface	calculator.java
	

	The calculator interface provides a description of the 5 remote 
	arithmetic methods available as part of the service provided
	by the remote object calculatorimpl. Note carefully that the interface
	extends remote and each methods throws a remote exception.
*/	


public interface calculator 
          extends java.rmi.Remote {	
    

    // add takes two long values, adds them together and returns the resulting
    // long value

    public long add(long a, long b)
        throws java.rmi.RemoteException;

    // sub takes two long values: a and b. It subtracts b from a and returns the
    // resulting long value

    public long sub(long a, long b)
        throws java.rmi.RemoteException;

    // mul takes two long values and multiplies them together. The resulting long
    // value is returned    

    public long mul(long a, long b)
        throws java.rmi.RemoteException;
 
    // div takes two long values a and b. a is divided by b and the resulting long
    // value is returned 
    
    public long div(long a, long b)
        throws java.rmi.RemoteException;

    // pow takes a long value a and an int value b. Where a^b is performed 
    
    public long pow(long a, int b)
	throws java.rmi.RemoteException;

}

