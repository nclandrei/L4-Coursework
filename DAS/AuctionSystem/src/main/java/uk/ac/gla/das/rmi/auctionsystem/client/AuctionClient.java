package uk.ac.gla.das.rmi.auctionsystem.client;

import uk.ac.gla.das.rmi.auctionsystem.api.AuctionManager;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionParticipant;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.RMISocketFactory;
import java.util.InputMismatchException;
import java.util.Scanner;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

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

    private static final Scanner SCANNER = new Scanner(System.in);

    private static void performHelp() {
        System.out.println(COMMANDS_LIST);
    }

    private static void performCreate(AuctionParticipant user, AuctionManager manager) throws RemoteException {
        try {
            System.out.println("Please insert a title for the item:");
            String itemTitle = SCANNER.nextLine();
            System.out.println("Please insert the starting value for your item:");
            String startingValueString = SCANNER.nextLine();
            Scanner startingValueScanner = new Scanner(startingValueString);
            double startingValue = startingValueScanner.nextDouble();
            startingValueScanner.close();
            System.out.println("Please insert the closing time of the auction in the format (20-11-2016T02:55):");
            String closingTime = SCANNER.nextLine();
            long auctionId = manager.createAuction(itemTitle, startingValue, closingTime, user);
            if (auctionId == -1) {
                System.out.println("Unfortunately, auction could not be created! Please try again.");
            } else {
                System.out.println("Auction with ID " + auctionId + " has just been created!");
            }
        } catch (InputMismatchException ex) {
            System.err.println("You typed a wrong input. Please try again!");
        }
    }

    private static void performList(AuctionManager manager) throws RemoteException {
        System.out.println(manager.getAllAuctions());
    }

    private static void performBid(AuctionParticipant user, AuctionManager manager) throws RemoteException {
        try {
            System.out.println("Please insert the auction ID you want to bid for:");
            String idToString = SCANNER.nextLine();
            Scanner scanId = new Scanner(idToString);
            long auctionId = scanId.nextLong();
            scanId.close();
            System.out.println("Please insert the amount you would like to bid:");
            String valueToString = SCANNER.nextLine();
            Scanner scanValue = new Scanner(valueToString);
            double bidAmount = scanValue.nextDouble();
            scanValue.close();
            manager.placeBid(user, auctionId, bidAmount);
        } catch (InputMismatchException ex) {
            System.err.println("You typed a wrong input. Please try again!");
        }
    }

    private static void performRestore(AuctionParticipant user, AuctionManager manager) throws RemoteException {
        manager.restoreState(user);
    }

    private static void performSave(AuctionParticipant user, AuctionManager manager) throws RemoteException {
        manager.saveState(user);
    }

    private static void performDisplay(AuctionManager manager) throws RemoteException {
        try {
            System.out.println("Please insert the ID of the auction you would like to see:");
            String idToString = SCANNER.nextLine();
            Scanner scanId = new Scanner(idToString);
            long id = scanId.nextLong();
            scanId.close();

            String auctionDisplayInfo = manager.getAuctionDetails(id);
            if (auctionDisplayInfo == null) {
                System.out.println("Unfortunately, auction with ID " + id + " could not be retrieved.");
            } else {
                System.out.println(auctionDisplayInfo);
            }
        } catch (InputMismatchException ex) {
            System.err.println("You typed a wrong input. Please try again!");
        }
    }

    private static void performQuit() {
        System.err.println("Stopping client...");
        System.exit(0);
    }

    private static boolean doesUserHaveSavedState(String name) {
        String userNameWithoutSpaces = name.replaceAll("\\s+", "").toLowerCase();
        File resourcesFolder =
                new File(String.format("%s/src/main/resources", new File(".").getAbsolutePath()));
        File[] savedStates = resourcesFolder.listFiles();
        for (File state : savedStates) {
            String fileName = state.getName();
            if (fileName.startsWith(userNameWithoutSpaces)) {
                return true;
            }
        }
        return false;
    }

    public static void main(String[] args) {
        AuctionParticipant auctionParticipant = null;
        AuctionManager auctionManager = null;

        if (args.length < 2) {
            System.err.println("Server IP address and port need to be specified as arguments in the form: " +
                    String.format("%s %s", "192.168.1.1", "1099") + "\n ! Please try again...");
            System.exit(-1);
        }

        try {
            System.out.println("Hi, nice to meet you! Could you please tell me your name?");
            String name = SCANNER.nextLine();
            System.out.println("Great, " + name + "!");
            auctionParticipant = new AuctionParticipantImpl(name);
            auctionManager = (AuctionManager)
                    Naming.lookup(String.format("rmi://%s:%s/AuctionServerService", args[0], args[1]));
            if (doesUserHaveSavedState(name)) {
                System.out.println("I found a state you saved previously. Would you like to restore it? (Yes or No)");
                String restore = SCANNER.nextLine();
                if (restore.equalsIgnoreCase("yes")) {
                    performRestore(auctionParticipant, auctionManager);
                }
            }
            
            ScheduledExecutorService exec = Executors.newSingleThreadScheduledExecutor();
            AuctionManager finalAuctionManager = auctionManager;
            exec.scheduleAtFixedRate(() -> {
                try {
                    finalAuctionManager.serverFailureDetector();
                }
                 catch (RemoteException e) {
                    System.err.println("Server failed... Shutting down client.");
                     performQuit();
                    e.printStackTrace();
                }
            }, 0, 5, TimeUnit.SECONDS);

        } catch (Exception ex) {
            ex.printStackTrace();
        }

        System.out.println("Welcome to the most awesome Auction System ever implemented in " +
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
        } catch (RemoteException ex) {
            System.err.println("Could not perform operation... Shutting down client.");
        }
    }
}