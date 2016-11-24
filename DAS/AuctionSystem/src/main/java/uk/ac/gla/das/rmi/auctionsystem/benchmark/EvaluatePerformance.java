package uk.ac.gla.das.rmi.auctionsystem.benchmark;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.chart.BarChart;
import javafx.scene.chart.CategoryAxis;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.XYChart;
import javafx.stage.Stage;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionManager;
import uk.ac.gla.das.rmi.auctionsystem.api.AuctionParticipant;
import uk.ac.gla.das.rmi.auctionsystem.client.AuctionParticipantImpl;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.util.Random;

/**
 * Class defined for testing performance when:
 *     - both server and clients are running locally
 *     - server and clients are spread across the network
 */
public class EvaluatePerformance extends Application {
    private final static String createColumn = "Create";
    private final static String bidColumn = "Bid";
    private final static String displayColumn = "Display";
    private final static String listAllColumn = "ListAll";
    private final static String saveStateColumn = "SaveState";
    private final static String restoreStateColumn = "RestoreState";
    private static AuctionManager auctionManager;
    private static Random randomizer;
    private static int numberOfExecutions;

    @Override
    public void start(Stage stage) {
        stage.setTitle("Benchmarking Auction System");
        final CategoryAxis xAxis = new CategoryAxis();
        final NumberAxis yAxis = new NumberAxis();
        final BarChart<String,Number> bc =
                new BarChart<>(xAxis,yAxis);
        bc.setTitle("Average Execution Times for Methods");
        xAxis.setLabel("Methods");
        yAxis.setLabel("Milliseconds");

        double averageBidExecutionTime = benchmarkBidding(numberOfExecutions);
        double averageCreateExecutionTime = benchmarkCreation(numberOfExecutions);
        double averageDisplayExecutionTime = benchmarkDisplaying(numberOfExecutions);
        double averageDisplayAllExecutionTime = benchmarkDisplayingAll(numberOfExecutions);
        double averageSaveStateExecutionTime = benchmarkSavingState(numberOfExecutions);
        double averageRestoreStateExecutionTime = benchmarkRestoringState(numberOfExecutions);


        XYChart.Series series1 = new XYChart.Series();
        series1.setName("Average Execution Time");
        series1.getData().add(new XYChart.Data(createColumn, averageCreateExecutionTime));
        series1.getData().add(new XYChart.Data(bidColumn, averageBidExecutionTime));
        series1.getData().add(new XYChart.Data(displayColumn, averageDisplayExecutionTime));
        series1.getData().add(new XYChart.Data(listAllColumn, averageDisplayAllExecutionTime));
        series1.getData().add(new XYChart.Data(saveStateColumn, averageSaveStateExecutionTime));
        series1.getData().add(new XYChart.Data(restoreStateColumn, averageRestoreStateExecutionTime));

        Scene scene  = new Scene(bc,800,600);
        bc.getData().addAll(series1);
        stage.setScene(scene);
        stage.show();
    }

    private static double benchmarkCreation (int numberOfExecutions) {
        try {
            long creationExecutionTimesSum = 0L;
            for (int i = 0; i < numberOfExecutions; ++i) {
                String itemTitle = "The User";
                double itemMinValue = 0;
                String closingTime = "01-01-2019T09:30";
                AuctionParticipant bidder = new AuctionParticipantImpl("TestUser" + randomizer.nextInt());
                long startTime = System.nanoTime();
                auctionManager.createAuction(itemTitle, itemMinValue, closingTime, bidder);
                long endTime = System.nanoTime();
                creationExecutionTimesSum += (endTime - startTime) / 1000000;
            }
            return ((double) creationExecutionTimesSum / numberOfExecutions);
        }
        catch (RemoteException ex) {
            return -1;
        }
    }

    private static double benchmarkBidding (int numberOfExecutions) {
        try {
            long biddingExecutionTimesSum = 0L;
            double bidAmount = 0;
            for (int i = 0; i < numberOfExecutions; ++i) {
                AuctionParticipant bidder = new AuctionParticipantImpl("TestUser" + randomizer.nextInt());
                long startTime = System.nanoTime();
                auctionManager.placeBid(bidder, 0, bidAmount++);
                long endTime = System.nanoTime();
                biddingExecutionTimesSum += (endTime - startTime) / 1000000;
            }
            return ((double) biddingExecutionTimesSum / numberOfExecutions);
        }
        catch (RemoteException ex) {
            return -1;
        }
    }

    private static double benchmarkDisplaying (int numberOfExecutions) {
        try {
            long displayExecutionTimesSum = 0L;
            int counter = 0;
            for (int i = 0; i < numberOfExecutions * numberOfExecutions; ++i) {
                long startTime = System.nanoTime();
                auctionManager.getAuctionDetails(counter++);
                long endTime = System.nanoTime();
                displayExecutionTimesSum += (endTime - startTime) / 1000000;
            }
            return ((double) displayExecutionTimesSum / numberOfExecutions);
        }
        catch (RemoteException ex) {
            return -1;
        }
    }

    private static double benchmarkDisplayingAll (int numberOfExecutions) {
        try {
            long displayingAllExecutionTimesSum = 0L;
            for (int i = 0; i < numberOfExecutions; ++i) {
                long startTime = System.nanoTime();
                auctionManager.getAllAuctions();
                long endTime = System.nanoTime();
                displayingAllExecutionTimesSum += (endTime - startTime) / 1000000;
            }
            return ((double) displayingAllExecutionTimesSum / numberOfExecutions);
        }
        catch (RemoteException ex) {
            return -1;
        }
    }

    private static double benchmarkSavingState (int numberOfExecutions) {
        try {
            long savingStateExecutionTimesSum = 0L;
            for (int i = 0; i < numberOfExecutions; ++i) {
                long startTime = System.nanoTime();
                auctionManager.saveState(new AuctionParticipantImpl("TestUser"));
                long endTime = System.nanoTime();
                savingStateExecutionTimesSum += (endTime - startTime) / 1000000;
            }
            return ((double) savingStateExecutionTimesSum / numberOfExecutions);
        }
        catch (RemoteException ex) {
            return -1;
        }
    }

    private static double benchmarkRestoringState (int numberOfExecutions) {
        try {
            long restoreStateExecutionTimesSum = 0L;
            for (int i = 0; i < numberOfExecutions; ++i) {
                long startTime = System.nanoTime();
                auctionManager.restoreState();
                long endTime = System.nanoTime();
                restoreStateExecutionTimesSum += (endTime - startTime) / 1000000;
            }
            return ((double) restoreStateExecutionTimesSum / numberOfExecutions);
        }
        catch (RemoteException ex) {
            return -1;
        }
    }

    public static void main(String[] args) {
        try {
            auctionManager = (AuctionManager)
                    Naming.lookup(String.format("rmi://%s:%s/AuctionServerService", args[0], args[1]));
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        randomizer = new Random();
        numberOfExecutions = Integer.parseInt(args[2]);
        launch(args);
    }
}