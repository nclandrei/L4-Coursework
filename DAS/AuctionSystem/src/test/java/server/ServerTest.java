package server;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionManager;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionParticipant;
import uk.ac.gla.das.rmi.auctionsystem.client.AuctionParticipantImpl;
import uk.ac.gla.das.rmi.auctionsystem.server.Auction;
import uk.ac.gla.das.rmi.auctionsystem.server.AuctionManagerImpl;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.rmi.Naming;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.ExportException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public class ServerTest {

    private static final String STATE_STORAGE_LOCATION =
            String.format("%s/src/main/resources/last_state.bin", new File(".").getAbsolutePath());
    private AuctionManager auctionManager;

    @Before
    public void init() throws Exception {
        Map<Long, Auction> auctionMap = new HashMap<>();
        AuctionParticipant auctionParticipant = new AuctionParticipantImpl("Test User");
        auctionMap.put(0L, new Auction("TestItem", 250, new Date(), 0, auctionParticipant));
        ObjectOutputStream fileWriter = new ObjectOutputStream(
                new BufferedOutputStream(
                        new FileOutputStream(STATE_STORAGE_LOCATION)));
        fileWriter.writeObject(auctionMap);
        fileWriter.writeObject(0);
        fileWriter.close();
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
    public void testBootstrap() throws Exception {
        assertTrue(auctionManager.getAllAuctions() != null);
    }


    @After
    public void tearDown() {
        File lastStateFile = new File(STATE_STORAGE_LOCATION);
        lastStateFile.delete();
    }
}
