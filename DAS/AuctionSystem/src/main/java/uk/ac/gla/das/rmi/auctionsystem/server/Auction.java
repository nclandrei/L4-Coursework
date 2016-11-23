package uk.ac.gla.das.rmi.auctionsystem.server;


import uk.ac.gla.das.rmi.auctionsystem.api.AuctionParticipant;
import uk.ac.gla.das.rmi.auctionsystem.common.DateUtility;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

public class Auction implements Serializable {

    private static final long serialVersionUID = -8663379121492277431L;
    private String itemTitle;
    private double itemValue;
    private Date closingTime;
    private long id;
    private Map<String, AuctionParticipant> biddersMap;
    private AuctionParticipant owner;
    private AuctionParticipant highestBidder;
    private boolean isClosed;

    public Auction (String itemTitle, double itemMinimumValue, Date closingTime, long id, AuctionParticipant owner) {
        this.itemTitle = itemTitle;
        this.itemValue = itemMinimumValue;
        this.closingTime = closingTime;
        this.id = id;
        this.owner = owner;
        this.biddersMap = new HashMap<>();

        this.setTimer();

        if (this.isClosed) {
            try {
                owner.notify("You have entered an invalid closing time!");
            }
            catch (RemoteException ex) {
                ex.printStackTrace();
            }
        }
    }

    public synchronized void setTimer () {
        if (this.closingTime.before(new Date())) {
            this.isClosed = true;
        }
        else {
            this.isClosed = false;
            (new Timer(true)).schedule(new TimerTask() {
                @Override
                public void run() {
                    System.out.println("Timer started!");
                    Auction.this.closeAuction();
                }
            }, closingTime);
        }
    }

    public synchronized String getShortDisplayInfo () {
        return String.format("%d, %s, %.2f, %s, %s", id, itemTitle,
                itemValue, closingTime, Boolean.toString(isClosed));
    }

    public synchronized String getFullDisplayInfo () {
        StringBuilder sb = new StringBuilder();
        SimpleDateFormat dateFormat = DateUtility.getDateFormat();
        sb.append ("ID -- " + this.id + "\n");
        sb.append ("Item Title -- " + this.itemTitle + "\n");
        sb.append ("Item Value -- " + this.itemValue + "\n");
        sb.append ("Closing Time -- " + dateFormat.format(this.closingTime) + "\n");
        sb.append ("Number of Bidders -- " + this.biddersMap.size() + "\n");
        if (this.isClosed) {
            try {
                sb.append(this.getAuctionResult());
            }
            catch (RemoteException ex) {
                ex.printStackTrace();
            }
        }
        return sb.toString();
    }

    public synchronized Date getClosingTime () {
        return this.closingTime;
    }

    private String getAuctionResult () throws RemoteException {
        if (highestBidder == null) {
            return "Price hasn't been met.";
        }
        else {
            return String.format("Purchased by %s for the price %sGBP.",
                    this.highestBidder.getName(), this.itemValue);
        }
    }

    public synchronized void bid (AuctionParticipant bidder, double bidAmount) {
        try {
            if (this.isClosed) {
                bidder.notify("Sorry, auction with ID " + this.id + " has been closed!");
                return;
            }
            if (bidAmount <= this.itemValue) {
                bidder.notify(String.format("You have not met current value of %.2f. Please bid higher!",
                        this.itemValue));
                return;
            }
            biddersMap.put(bidder.getId(), bidder);
            this.highestBidder = bidder;
            this.itemValue = bidAmount;
            bidder.notify(String.format("Your bid of %.2f has been accepted!", bidAmount));
        }
        catch (RemoteException ex) {
            ex.printStackTrace();
        }
    }

    private synchronized void closeAuction () {
        System.out.println ("Trying to close auction with ID " + this.id);
        this.isClosed = true;
        try {
            if (this.highestBidder == null) {
                System.out.println ("There was no bidder for auction with ID " + this.id);
                this.owner.notify("We are sorry to announce that your item -" + this.itemTitle + "- was not sold.");
                for (AuctionParticipant bidder : this.biddersMap.values()) {
                    bidder.notify(String.format("Item %s was not sold. Nobody met starting value %.2f",
                            this.itemTitle, this.itemValue));
                }
            }
            else {
                System.out.println (String.format("Auction with ID %d was won with the amount %.2f by %s.",
                                    this.id, this.itemValue, this.highestBidder.getName()));
                this.owner.notify(String.format("Congratulations! Your item %s was sold to %s with the amount %.2f.",
                        this.itemTitle, this.highestBidder.getName(), this.itemValue));
                for (Map.Entry<String, AuctionParticipant> entry : biddersMap.entrySet()) {
                    AuctionParticipant bidder = entry.getValue();
                    String bidderId = entry.getKey();
                    if (bidder == this.highestBidder) {
                        bidder.notify(String.format("Congratulations - you won the item %s for %.2f!",
                                this.itemTitle, this.itemValue));
                    }
                    else {
                        bidder.notify(String.format("Item %s was purchased by %s for %.2f",
                                this.itemTitle, this.highestBidder.getName(), this.itemValue));
                    }
                }
            }
        }
        catch (RemoteException ex) {
            System.err.println ("Could not close auction with ID " + this.id);
        }
    }
}
