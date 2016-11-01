/*
	Code: calculator server		CalculatorServer.java
	

	Server code for hosting the CalculatorImpl object
*/


import java.rmi.Naming;	//Import naming classes to bind to rmiregistry
import java.rmi.server.UnicastRemoteObject;


public class calculatorserver {
	static int port = 1099;
   //calculatorserver constructor
   public calculatorserver() {
     
     //Construct a new CalculatorImpl object and bind it to the local rmiregistry
     //N.b. it is possible to host multiple objects on a server by repeating the
     //following method. 

     try {
       	//calculator c = new calculatorimpl();
       	calculatorimpl ci = new calculatorimpl();
       	calculator c = (calculator) UnicastRemoteObject.exportObject(ci, 0);
       	Naming.rebind("rmi://localhost:" + port + "/CalculatorService", c);
     } 
     catch (Exception e) {
       System.out.println("Server Error: " + e);
     }
   }

   public static void main(String args[]) {
     	//Create the new Calculator server
	if (args.length == 1)
		port = Integer.parseInt(args[0]);
	
	new calculatorserver();
   }
}
