package uk.ac.gla.auctionsystem.common;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface AuctionManager extends Remote {

    public long createAuction(String itemTitle, double itemMinVal,
                              String closingTime, AuctionUser user) throws RemoteException;

    public String getAllAuctions () throws RemoteException;

    public boolean placeBid (AuctionUser user, long auctionId, double bidAmount) throws RemoteException;

    public String getAuctionDetails (long id) throws RemoteException;

    public boolean restoreState (AuctionUser user) throws RemoteException;

    public boolean saveState (AuctionUser user) throws RemoteException;
}
