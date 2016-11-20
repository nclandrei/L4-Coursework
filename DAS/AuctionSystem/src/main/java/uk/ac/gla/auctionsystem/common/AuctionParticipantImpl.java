package uk.ac.gla.auctionsystem.common;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.UUID;

public class AuctionParticipantImpl extends UnicastRemoteObject implements AuctionParticipant {

    private static final long serialVersionUID = -2806330933785406583L;
    private UUID id;
    private String name;
    private boolean isAdmin;

    public AuctionParticipantImpl(String name, boolean isAdmin) throws RemoteException {
        this.name = name;
        this.isAdmin = isAdmin;
        this.id = UUID.randomUUID();
    }

    @Override
    public String getName() throws RemoteException {
        return this.name;
    }

    @Override
    public String getId() throws RemoteException {
        return this.id.toString();
    }

    @Override
    public boolean isAdmin() throws RemoteException {
        return false;
    }

    @Override
    public void notify(String info) throws RemoteException {
        System.out.println("INFO --> " + info);
    }
}
