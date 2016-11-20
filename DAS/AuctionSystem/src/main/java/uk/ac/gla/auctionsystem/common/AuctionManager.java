package uk.ac.gla.auctionsystem.common;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface AuctionManager extends Remote {

    public long createAuction(String itemTitle, double itemMinVal,
                              String closingTime, AuctionParticipant user) throws RemoteException;

    public String getAllAuctions () throws RemoteException;

    public boolean placeBid (AuctionParticipant user, long auctionId, double bidAmount) throws RemoteException;

    public String getAuctionDetails (long id) throws RemoteException;

    public boolean restoreState (AuctionParticipant user) throws RemoteException;

    public boolean saveState (AuctionParticipant user) throws RemoteException;
}
