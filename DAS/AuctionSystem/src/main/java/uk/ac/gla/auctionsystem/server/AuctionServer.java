package uk.ac.gla.auctionsystem.server;


import uk.ac.gla.auctionsystem.common.AuctionManager;
import uk.ac.gla.auctionsystem.common.AuctionManagerImpl;

import java.rmi.Naming;

public class AuctionServer {

    public static void main (String[] args) {
        if (System.getSecurityManager() == null) {
            System.setSecurityManager(new SecurityManager());
        }
        try {
            AuctionManager auctionManager = new AuctionManagerImpl();
            Naming.rebind("rmi://localhost/AuctionServer", auctionManager);
            System.out.println ("Welcome to the most awesome Auction System ever implemented in Java RMI!");
        }
        catch (Exception ex) {
            System.err.println ("Could not launch server... Please try again.");
            ex.printStackTrace();
        }
    }
}
