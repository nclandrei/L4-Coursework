package uk.ac.gla.das.rmi.auctionsystem.api;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface AuctionParticipant extends Remote {

    String getName () throws RemoteException;

    String getId () throws RemoteException;

    void notify (String info) throws RemoteException;

}
