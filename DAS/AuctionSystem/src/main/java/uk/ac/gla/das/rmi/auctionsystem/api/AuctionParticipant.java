package uk.ac.gla.das.rmi.auctionsystem.api;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface AuctionParticipant extends Remote {

    public String getName () throws RemoteException;

    public String getId () throws RemoteException;

    public void notify (String info) throws RemoteException;

    public boolean isAdmin () throws RemoteException;

}