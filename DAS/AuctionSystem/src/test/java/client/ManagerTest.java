package client;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

import uk.ac.gla.das.rmi.auctionsystem.api.AuctionManager;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionParticipant;
import uk.ac.gla.das.rmi.auctionsystem.client.AuctionParticipantImpl;
import uk.ac.gla.das.rmi.auctionsystem.server.AuctionManagerImpl;

import java.io.File;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.ExportException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

public class ManagerTest {

    private static final int NUMBER_OF_USERS = 5;
    private static final int INITIAL_AUCTIONS_COUNT = 20;
    private static final int PERFORM_CREATE_COUNT = 25;
    private static final int PERFORM_BID_COUNT = 30;
    private static final int PERFORM_LIST_COUNT = 45;
    private static final int PERFORM_DISPLAY_COUNT = 25;
    private static final String RESOURCES_FOLDER_PATH =
            String.format("%s/src/main/resources", new File(".").getAbsolutePath());

    private static final String[] CLOSING_DATES =
            new String[]{"01-01-2017T12:00", "02-04-2017T15:50", "03-05-2017T09:09", "06-07-2017T16:30"};

    private AuctionManager auctionManager;
    private List<AuctionParticipant> participantList;
    private Random randomizer;

    @Before
    public void init() throws Exception {
        randomizer = new Random();
        participantList = new ArrayList<>(NUMBER_OF_USERS);
        try {
            LocateRegistry.createRegistry(1099);
            auctionManager = new AuctionManagerImpl();
            Naming.rebind("rmi://localhost:1099/AuctionServerService", auctionManager);
            if (System.getSecurityManager() == null) {
                System.setSecurityManager(new SecurityManager());
            }
        }
        catch (ExportException ex) {
            LocateRegistry.getRegistry(1099);
        }

        auctionManager = (AuctionManager) Naming.lookup
                (String.format("rmi://%s:%d/AuctionServerService", "localhost", 1099));
        for (int i = 0; i < NUMBER_OF_USERS; ++i) {
            participantList.add(new AuctionParticipantImpl("TestUser" + randomizer.nextInt()));
        }
        initializeAuctions();
    }

    private void initializeAuctions() throws RemoteException {
        for (int i = 0; i < INITIAL_AUCTIONS_COUNT; ++i) {
            String itemTitle = "Test Item Title" + randomizer.nextInt();
            double itemMinValue = randomizer.nextDouble();
            String closingDate = CLOSING_DATES[randomizer.nextInt(CLOSING_DATES.length)];
            int randomParticipantId = randomizer.nextInt(participantList.size());
            auctionManager.createAuction(itemTitle, itemMinValue,
                    closingDate, participantList.get(randomParticipantId));
        }
    }

    @Test
    public void testPerformCreate() throws RemoteException {
        for (int i = 0; i < PERFORM_CREATE_COUNT; ++i) {
            String itemTitle = "Test Item Title" + randomizer.nextInt();
            double itemMinValue = randomizer.nextDouble();
            String closingDate = CLOSING_DATES[randomizer.nextInt(CLOSING_DATES.length)];
            int randomParticipantId = randomizer.nextInt(participantList.size());
            auctionManager.createAuction(itemTitle, itemMinValue,
                    closingDate, participantList.get(randomParticipantId));
            assertTrue(auctionManager.getAuctionDetails(INITIAL_AUCTIONS_COUNT + i) != null);
        }
    }

    @Test
    public void testPerformBid() throws RemoteException {
        for (int i = 0; i < PERFORM_BID_COUNT; ++i) {
            double bidAmount = randomizer.nextDouble() * 1000;
            long auctionId = ThreadLocalRandom.current().nextLong(INITIAL_AUCTIONS_COUNT);
            int participantId = randomizer.nextInt(NUMBER_OF_USERS);
            assertTrue(auctionManager.placeBid(participantList.get(participantId), auctionId, bidAmount));
        }
    }

    @Test
    public void testPerformList() throws RemoteException {
        for (int i = 0; i < PERFORM_LIST_COUNT; ++i) {
            assertTrue(auctionManager.getAllAuctions() != null);
        }
    }

    @Test
    public void testPerformDisplay() throws RemoteException {
        for (int i = 0; i < PERFORM_DISPLAY_COUNT; ++i) {
            long auctionId = ThreadLocalRandom.current().nextLong(INITIAL_AUCTIONS_COUNT);
            assertTrue(auctionManager.getAuctionDetails(auctionId) != null);
        }
    }

    @Test
    public void testPerformSaveState() throws RemoteException {
        AuctionParticipant participant = new AuctionParticipantImpl("TestUser");
        auctionManager.saveState(participant);
        File stateBinFile = new File(String.format("%s/testuser_last_state.bin", RESOURCES_FOLDER_PATH));
        assertTrue(stateBinFile.exists());
    }

    @Test
    public void testPerformRestoreState() throws RemoteException {
        AuctionParticipant participant = new AuctionParticipantImpl("TestUser");
        auctionManager.saveState(participant);
        assertTrue(auctionManager.restoreState());
    }

    @After
    public void tearDown() {
        File resourcesFolder = new File(RESOURCES_FOLDER_PATH);
        File[] binFiles = resourcesFolder.listFiles();
        for (File stateFile : binFiles) {
            stateFile.delete();
        }
    }
}
