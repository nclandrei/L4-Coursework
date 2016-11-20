package uk.ac.gla.auctionsystem.client;

import uk.ac.gla.auctionsystem.common.AuctionManager;
import uk.ac.gla.auctionsystem.common.AuctionUser;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.util.Scanner;

public class AuctionClient {

    private static final String COMMANDS_LIST =
            "help     -->    Show available commands \n" +
            "create   -->    Create a specific auction item \n" +
            "list     -->    List all auctions \n" +
            "bid      -->    Place bid for a specific item" +
            "restore  -->    Restore system to last save's state" +
            "save     -->    Save system state" +
            "info     -->    Get info regarding specific auction" +
            "quit     -->    Quit";

    private static final Scanner SCANNER = new Scanner (System.in);

    private static void performHelp () {
        System.out.println (COMMANDS_LIST);
    }

    private static void performCreate (AuctionUser user, AuctionManager manager) {

    }

    private static void performList (AuctionManager manager) {

    }

    private static void performBid (AuctionUser user, AuctionManager manager) {

    }

    private static void performRestore (AuctionUser user) {

    }

    private static void performSave (AuctionUser user) {

    }

    private static void performInfo (AuctionManager manager) {

    }

    private static void performQuit () {
        System.err.println ("Stopping client...");
        System.exit(0);
    }

    public static void main (String[] args) {
        AuctionUser auctionUser = null;
        AuctionManager auctionManager = null;

        if (args.length < 1) {
            System.err.println ("Server IP address needs to be specified as an argument in the form: " +
                                String.format("%s", "192.168.1.1") + "\n ! Please try again...");
            System.exit(-1);
        }

        try {
            System.out.println("Hi, nice to meet you! Could you please tell me your name?");
            String name = SCANNER.next();
            System.out.println("Great, " + name + "! Could you please tell me if you're an admin?");
            boolean isAdmin = SCANNER.nextBoolean();
            auctionUser = new AuctionUser(name, isAdmin);
            auctionManager = (AuctionManager) Naming.lookup("rmi://" + args[0] + "/AuctionManagerService");
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }

        System.out.println ("Welcome to the most awesome Auction System ever implemented in " +
                            "Java RMI!\n Please type help to find out what can you do! Enjoy!");

        while (true) {
            System.out.print(">> ");
            String command = SCANNER.next();

            if (command.startsWith("help")) {
                performHelp();
            }
            else if (command.startsWith("create")) {
                performCreate(auctionUser, auctionManager);
            }
            else if (command.startsWith("list")) {
                performList(auctionManager);
            }
            else if (command.startsWith("bid")) {
                performBid(auctionUser, auctionManager);
            }
            else if (command.startsWith("restore")) {
                performRestore(auctionUser);
            }
            else if (command.startsWith("save")) {
                performSave(auctionUser);
            }
            else if (command.startsWith("info")) {
                performInfo(auctionManager);
            }
            else if (command.startsWith("quit")) {
                performQuit();
            }
            else {
                System.err.println("Wrong command typed... If you forgot available commands, " +
                                   "please type help!");
            }
        }
    }

}
