package uk.ac.gla.das.rmi.auctionsystem.server;

import uk.ac.gla.das.rmi.auctionsystem.api.AuctionManager;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionParticipant;
import uk.ac.gla.das.rmi.auctionsystem.common.DateUtility;

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
    private static final String LAST_STATE_FILENAME = "last_state.bin";
    private static final int STORAGE_MINUTES = 30;

    private Long nextAuctionId;
    private Map<Long, Auction> auctionMap;

    public AuctionManagerImpl () throws RemoteException {
        super();
        this.auctionMap = new HashMap<>();
        nextAuctionId = 0L;
    }

    @Override
    public long createAuction(String itemTitle, double itemMinVal, String closingTime, AuctionParticipant user) throws RemoteException {
        long id;
        synchronized (nextAuctionId) {
            id = this.nextAuctionId;
            this.nextAuctionId++;
        }
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

        Auction auction = new Auction(itemTitle, itemMinVal, closingTimeDate, id, user);
        System.out.println("Auction with ID " + id + " has just been created.");
        auctionMap.put(id, auction);
        return id;
    }

    @Override
    public String getAllAuctions() throws RemoteException {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("Item-ID\tItem-Title\tItem-Value\tClosing-Time\tIs-Closed\n\n");
        Calendar calendar = new GregorianCalendar();
        for (Map.Entry<Long, Auction> entry : auctionMap.entrySet()) {
            Long id = entry.getKey();
            Auction auction = entry.getValue();
            calendar.setTime(auction.getClosingTime());
            calendar.add(GregorianCalendar.MINUTE, STORAGE_MINUTES);
            if ((new GregorianCalendar().after(calendar))) {
                auctionMap.remove(id);
                continue;
            }
            stringBuilder.append(auction.getShortDisplayInfo() + "\n");
        }
        return stringBuilder.toString();
    }

    @Override
    public boolean placeBid(AuctionParticipant user, long auctionId, double bidAmount) throws RemoteException {
        Auction auction = auctionMap.get(auctionId);
        if (auction == null) {
            user.notify("Sorry, the specified ID is not correct.");
            return false;
        }
        auction.bid(user, bidAmount);
        return true;
    }

    @Override
    public String getAuctionDetails(long id) throws RemoteException {
        Auction auction = auctionMap.get(id);
        if (auction == null) {
            return null;
        }
        else {
            return auction.getFullDisplayInfo();
        }
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