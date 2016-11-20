package uk.ac.gla.auctionsystem.common;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;


public class AuctionManagerImpl extends UnicastRemoteObject implements AuctionManager {

    private static final long serialVersionUID = 2974850963457238618L;


    @Override
    public long createAuction(String itemTitle, double itemMinVal, String closingTime, AuctionParticipant user) throws RemoteException {
        return 0;
    }

    @Override
    public String getAllAuctions() throws RemoteException {
        return null;
    }

    @Override
    public boolean placeBid(AuctionParticipant user, long auctionId, double bidAmount) throws RemoteException {
        return false;
    }

    @Override
    public String getAuctionDetails(long id) throws RemoteException {
        return null;
    }

    @Override
    public boolean restoreState(AuctionParticipant user) throws RemoteException {
        return false;
    }

    @Override
    public boolean saveState(AuctionParticipant user) throws RemoteException {
        return false;
    }
}
