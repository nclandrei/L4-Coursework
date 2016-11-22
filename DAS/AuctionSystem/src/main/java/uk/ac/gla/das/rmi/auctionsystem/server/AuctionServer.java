package uk.ac.gla.das.rmi.auctionsystem.server;

import uk.ac.gla.das.rmi.auctionsystem.api.AuctionManager;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;

public class AuctionServer {

    public static void main (String[] args) {
        try {
            LocateRegistry.createRegistry(1099);
        }
        catch (RemoteException ex) {
            ex.printStackTrace();
        }
        if (System.getSecurityManager() == null) {
            System.setSecurityManager(new SecurityManager());
        }
        System.setProperty("java.rmi.server.hostname", "localhost");
        try {
            AuctionManager auctionManager = new AuctionManagerImpl();
            Naming.rebind("rmi://localhost:1099/AuctionServerService", auctionManager);
            System.out.println ("Welcome to the most awesome Auction System ever implemented in Java RMI!");
            boolean canRestore = auctionManager.restoreState();
            if (canRestore) {
                System.out.println ("Bootstrapped from state saved to permanent storage.");
            }
        }
        catch (Exception ex) {
            System.err.println ("Could not launch server... Please try again.");
            ex.printStackTrace();
        }
    }
}
