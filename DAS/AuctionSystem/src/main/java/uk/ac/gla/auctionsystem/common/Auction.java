package uk.ac.gla.auctionsystem.common;


import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public class Auction implements Serializable {

    private String itemTitle;
    private double itemMinimumValue;
    private Date closingTime;
    private long id;
    private Map<String, AuctionUser> biddersMap;
    private AuctionUser owner;
    private String highestBidderName;
    private boolean isClosed;

    public Auction (String itemTitle, double itemMinimumValue, Date closingTime, long id, AuctionUser owner) {
        this.itemTitle = itemTitle;
        this.itemMinimumValue = itemMinimumValue;
        this.closingTime = closingTime;
        this.id = id;
        this.owner = owner;
        this.biddersMap = new HashMap<>();

        if (this.isClosed) {
            try {
                owner.inform("You have entered an invalid closing time!");
            }
            catch (RemoteException ex) {
                ex.printStackTrace();
            }
        }
    }

    public 

}
