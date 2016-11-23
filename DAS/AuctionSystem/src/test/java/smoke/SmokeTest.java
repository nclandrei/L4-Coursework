package smoke;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionManager;
import uk.ac.gla.das.rmi.auctionsystem.client.AuctionParticipantImpl;
import uk.ac.gla.das.rmi.auctionsystem.server.AuctionManagerImpl;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.ExportException;

/**
 * Testing to see if initializing a server and a client allows
 * connection between the two
 */
public class SmokeTest {

    private AuctionManager auctionManager;

    @Before
    public void init() throws Exception {
        try {
            LocateRegistry.createRegistry(1099);
        }
        catch (ExportException ex) {
            LocateRegistry.getRegistry(1099);
        }
        auctionManager = new AuctionManagerImpl();
        Naming.rebind("rmi://localhost:1099/AuctionServerService", auctionManager);
    }

    @Test
    public void isServerRunning () throws RemoteException{
        this.auctionManager.createAuction("testTitle", 25, "29-11-2016T03:00", new AuctionParticipantImpl("testName"));
        assertTrue(!this.auctionManager.getAuctionDetails(0).isEmpty());
    }

}
