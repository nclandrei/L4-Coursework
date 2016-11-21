package uk.ac.gla.das.rmi.auctionsystem.client;

import uk.ac.gla.das.rmi.auctionsystem.api.AuctionParticipant;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.UUID;

public class AuctionParticipantImpl extends UnicastRemoteObject implements AuctionParticipant {

    private static final long serialVersionUID = -2806330933785406583L;
    private UUID id;
    private String name;

    public AuctionParticipantImpl(String name) throws RemoteException {
        this.name = name;
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
    public void notify(String info) throws RemoteException {
        System.out.println("INFO --> " + info);
    }
}
