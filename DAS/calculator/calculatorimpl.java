/*
	Code: CalculatorImpl remote object	calculatorimpl.java
	

	Contains the arithmetic methods that can be remotley invoked
*/

// The implementation Class must implement the rmi interface (calculator)
// and be set as a Remote object on a server

public class calculatorimpl implements calculator {

    // Implementations must have an explicit constructor
    // in order to declare the RemoteException exception

    public calculatorimpl()
        throws java.rmi.RemoteException {
        super();
    }

    // Implementation of the add method
    // The two long parameters are added added and the result is retured
    public long add(long a, long b)
        throws java.rmi.RemoteException {
        return a + b;
    }

    // Subtract the second parameter from the first and return the result	
    public long sub(long a, long b)
        throws java.rmi.RemoteException {
        return a - b;
    }

    // Multiply the two parameters and return the result
    public long mul(long a, long b)
        throws java.rmi.RemoteException {
        return a * b;
    }

    // Divide first parameter by the second and return the result
    public long div(long a, long b)
        throws java.rmi.RemoteException {
        return a / b;
    }

    // Recursive power definition
    public long pow(long a, int b)
	throws java.rmi.RemoteException {
	
	if (b==0)
		return 1;
	else
		return a*pow(a, b-1); 
    }
}

