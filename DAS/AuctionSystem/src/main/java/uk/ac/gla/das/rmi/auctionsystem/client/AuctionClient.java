package uk.ac.gla.das.rmi.auctionsystem.client;

import uk.ac.gla.das.rmi.auctionsystem.api.AuctionManager;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionParticipant;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.util.Scanner;

public class AuctionClient {

    private static final String COMMANDS_LIST =
            "\n" + "help     -->    Show available commands \n" +
            "create   -->    Create a specific auction item \n" +
            "list     -->    List all auctions \n" +
            "bid      -->    Place bid for a specific item \n" +
            "restore  -->    Restore system to last save's state \n" +
            "save     -->    Save system state \n" +
            "display  -->    Get information regarding specific auction \n" +
            "quit     -->    Quit \n";

    private static final Scanner SCANNER = new Scanner (System.in);

    private static void performHelp () {
        System.out.println (COMMANDS_LIST);
    }

    private static void performCreate (AuctionParticipant user, AuctionManager manager) throws RemoteException {
        System.out.print ("Please insert a title for the item \n ");
        String itemTitle = SCANNER.nextLine();
        System.out.print ("Please insert the starting value for your item \n ");
        String startingValueString = SCANNER.nextLine();
        Scanner startingValueScanner = new Scanner(startingValueString);
        double startingValue = startingValueScanner.nextDouble();
        startingValueScanner.close();
        System.out.print ("Please insert the closing time of the auction in the format (20-11-2016T02:55) \n ");
        String closingTime = SCANNER.nextLine();
        long auctionId = manager.createAuction(itemTitle, startingValue, closingTime, user);
        if (auctionId == -1) {
            System.out.println ("Unfortunately, auction could not be created! Please try again.");
        }
        else {
            System.out.println ("Auction with ID " + auctionId + " has just been created!");
        }
    }

    private static void performList (AuctionManager manager) throws RemoteException {
        System.out.println (manager.getAllAuctions());
    }

    private static void performBid (AuctionParticipant user, AuctionManager manager) throws RemoteException {
        System.out.print ("Please insert the auction ID you want to bid for \n ");
        String idToString = SCANNER.nextLine();
        Scanner scanId = new Scanner(idToString);
        long auctionId = scanId.nextLong();
        scanId.close();
        System.out.print ("Please insert the amount you would like to bid \n ");
        String valueToString = SCANNER.nextLine();
        Scanner scanValue = new Scanner(valueToString);
        double bidAmount = scanValue.nextDouble();
        scanValue.close();
        manager.placeBid(user, auctionId, bidAmount);
    }

    private static void performRestore (AuctionParticipant user, AuctionManager manager) throws RemoteException {
        manager.restoreState(user);
    }

    private static void performSave (AuctionParticipant user, AuctionManager manager) throws RemoteException {
        manager.saveState(user);
    }

    private static void performDisplay (AuctionManager manager) throws RemoteException {
        System.out.print ("Please insert the ID of the auction you would like to see \n ");
        String idToString = SCANNER.nextLine();
        Scanner scanId = new Scanner(idToString);
        long id = scanId.nextLong();
        scanId.close();

        String auctionDisplayInfo = manager.getAuctionDetails(id);
        if (auctionDisplayInfo == null) {
            System.out.println ("Unfortunately, auction with ID " + id + " could not be retrieved.");
        }
        else {
            System.out.println (auctionDisplayInfo);
        }
    }

    private static void performQuit () {
        System.err.println ("Stopping client...");
        System.exit(0);
    }

    public static void main (String[] args) {
        AuctionParticipant auctionParticipant = null;
        AuctionManager auctionManager = null;

        if (args.length < 1) {
            System.err.println ("Server IP address needs to be specified as an argument in the form: " +
                                String.format("%s", "192.168.1.1") + "\n ! Please try again...");
            System.exit(-1);
        }

        try {
            System.out.println("Hi, nice to meet you! Could you please tell me your name?");
            String name = SCANNER.nextLine();
            System.out.println("Great, " + name + "! Could you please tell me if you're an admin? (Yes or No)");
            String isUserAdmin = SCANNER.nextLine();
            boolean isAdmin;
            if (isUserAdmin.equalsIgnoreCase("Yes")) {
                isAdmin = true;
            }
            else {
                isAdmin = false;
            }
            auctionParticipant = new AuctionParticipantImpl(name, isAdmin);
            auctionManager = (AuctionManager) Naming.lookup("rmi://" + args[0] + ":1099/AuctionServerService");
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }

        System.out.println ("Welcome to the most awesome Auction System ever implemented in " +
                            "Java RMI!\nPlease type help to find out what can you do! Enjoy!");

        try {
            while (true) {
                String command = SCANNER.nextLine();

                if (command.startsWith("help")) {
                    performHelp();
                } else if (command.startsWith("create")) {
                    performCreate(auctionParticipant, auctionManager);
                } else if (command.startsWith("list")) {
                    performList(auctionManager);
                } else if (command.startsWith("bid")) {
                    performBid(auctionParticipant, auctionManager);
                } else if (command.startsWith("restore")) {
                    performRestore(auctionParticipant, auctionManager);
                } else if (command.startsWith("save")) {
                    performSave(auctionParticipant, auctionManager);
                } else if (command.startsWith("display")) {
                    performDisplay(auctionManager);
                } else if (command.startsWith("quit")) {
                    performQuit();
                } else {
                    System.err.println("Wrong command typed... If you forgot available commands, " +
                            "please type help!");
                }
            }
        }
        catch (RemoteException ex) {
            System.err.println ("Could not perform operation...");
        }
    }
}