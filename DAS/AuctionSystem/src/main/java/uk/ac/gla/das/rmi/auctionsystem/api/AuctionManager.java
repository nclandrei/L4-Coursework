package uk.ac.gla.das.rmi.auctionsystem.api;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface AuctionManager extends Remote {

    long createAuction(String itemTitle, double itemMinVal,
                              String closingTime, AuctionParticipant user) throws RemoteException;

    String getAllAuctions () throws RemoteException;

    boolean placeBid (AuctionParticipant user, long auctionId, double bidAmount) throws RemoteException;

    String getAuctionDetails (long id) throws RemoteException;

    boolean restoreState (AuctionParticipant user) throws RemoteException;

    boolean saveState (AuctionParticipant user) throws RemoteException;
}
