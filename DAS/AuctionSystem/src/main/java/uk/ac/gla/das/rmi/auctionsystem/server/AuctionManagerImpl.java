package uk.ac.gla.das.rmi.auctionsystem.server;

import uk.ac.gla.das.rmi.auctionsystem.api.AuctionManager;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionParticipant;
import uk.ac.gla.das.rmi.auctionsystem.common.DateUtility;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;


public class AuctionManagerImpl extends UnicastRemoteObject implements AuctionManager {

    private static final long serialVersionUID = 2974850963457238618L;
    private static final String STATE_STORAGE_LOCATION =
            String.format("%s/src/main/resources/last_state.bin", new File(".").getAbsolutePath());
    private static final int STORAGE_TIME_AMOUNT = 30;
    private static final int STORAGE_TIME_UNIT_TYPE = GregorianCalendar.MINUTE;


    private Long nextAuctionId;
    private Map<Long, Auction> auctionsMap;

    public AuctionManagerImpl () throws RemoteException {
        super();
        this.auctionsMap = new HashMap<>();
        nextAuctionId = 0L;
    }

    @Override
    public long createAuction(String itemTitle, double itemMinVal, String closingTime, AuctionParticipant user) throws RemoteException {
        Date closingTimeDate;
        try {
            SimpleDateFormat dateFormat = DateUtility.getDateFormat();
            closingTimeDate = dateFormat.parse(closingTime);
        }
        catch (ParseException ex) {
            user.notify("Sorry, the closing time could not be parsed.");
            return -1;
        }
        if (closingTimeDate.before(new Date())) {
            return -1;
        }
        long id;
        synchronized (nextAuctionId) {
            id = this.nextAuctionId;
            this.nextAuctionId++;
        }

        Auction auction = new Auction(itemTitle, itemMinVal, closingTimeDate, id, user);
        System.out.println("Auction with ID " + id + " has just been created.");
        auctionsMap.put(id, auction);
        return id;
    }

    @Override
    public String getAllAuctions() throws RemoteException {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("Item-ID\tItem-Title\tItem-Value\tClosing-Time\tIs-Closed\n\n");
        Calendar calendar = new GregorianCalendar();
        for (Map.Entry<Long, Auction> entry : auctionsMap.entrySet()) {
            Long id = entry.getKey();
            Auction auction = entry.getValue();
            calendar.setTime(auction.getClosingTime());
            calendar.add(STORAGE_TIME_UNIT_TYPE, STORAGE_TIME_AMOUNT);
            if ((new GregorianCalendar().after(calendar))) {
                auctionsMap.remove(id);
                continue;
            }
            stringBuilder.append(auction.getShortDisplayInfo()).append("\n");
        }
        return stringBuilder.toString();
    }

    @Override
    public boolean placeBid(AuctionParticipant user, long auctionId, double bidAmount) throws RemoteException {
        Auction auction = auctionsMap.get(auctionId);
        if (auction == null) {
            user.notify("Sorry, the specified ID is not correct.");
            return false;
        }
        auction.bid(user, bidAmount);
        return true;
    }

    @Override
    public String getAuctionDetails(long id) throws RemoteException {
        Auction auction = auctionsMap.get(id);
        if (auction == null) {
            return null;
        }
        else {
            return auction.getFullDisplayInfo();
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean restoreState() throws RemoteException {
        try {
            ObjectInputStream fileReader = new ObjectInputStream(
                    new BufferedInputStream(
                            new FileInputStream(STATE_STORAGE_LOCATION)));
            this.auctionsMap = (Map<Long, Auction>) fileReader.readObject();
            this.nextAuctionId = (Long) fileReader.readObject();
        }
        catch (ClassNotFoundException | IOException ex) {
            return false;
        }
        this.auctionsMap.values().forEach(Auction::setTimer);
        System.out.println("Last state available has been restored.");
        return true;
    }

    @Override
    public boolean saveState(AuctionParticipant user) throws RemoteException {
        try {
            ObjectOutputStream fileWriter = new ObjectOutputStream(
                    new BufferedOutputStream(
                            new FileOutputStream(STATE_STORAGE_LOCATION)));
            fileWriter.writeObject(auctionsMap);
            fileWriter.writeObject(nextAuctionId);
            fileWriter.close();
            System.out.println(String.format("Last state has been saved to resources folder by %s.", user.getName()));
            user.notify("You have just saved state on permanent storage.");
            return true;
        }
        catch (FileNotFoundException ex) {
            System.err.println ("File could not be reached.");
            ex.printStackTrace();
            return false;
        }
        catch (IOException ex) {
            System.err.println ("Could not write to file.");
            ex.printStackTrace();
            return false;
        }
    }

    @Override
    public void serverFailureDetector () {}
}
