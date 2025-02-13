package org.example.CinemaBooking.listener;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Main {
    //TODO FOR LOCAL CONF: "listener@localhost"
    //TODO FOR REMOTE CONF: "listener@10.2.1.41"
    private static final String node_name = "listener@10.2.1.41";
    private static final String cookie = "abcde";
    private static final String mailbox = "mbox";
    private static final int THREAD_POOL_SIZE = 5;

    public static void main(String[] args) throws IOException, OtpErlangDecodeException, OtpErlangExit {
        // i tried to try catch this, cannot find a nice way to do this tbh
        ExecutorService thPool = Executors.newFixedThreadPool(THREAD_POOL_SIZE);

        OtpNode otpNode = null;

        try {
            otpNode = new OtpNode(node_name, cookie);

            // exchange messages with erlang :
            // https://www.erlang.org/docs/27/apps/jinterface/assets/java/com/ericsson/otp/erlang/otpmbox
            OtpMbox otpMbox = otpNode.createMbox(mailbox);


            /*
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                thPool.shutdown();
                if (otpNode != null) {
                    otpNode.close();
                }
                System.out.println("Application shutdown complete.");
            }));
            */

            while (true) {
                try {
                    System.out.println("Receiving Erlang msg ...");
                    // receive can call exceptions
                    OtpErlangObject message = otpMbox.receive();
                    Runnable r = new ErlangMessageTask(message);
                    thPool.execute(r);
                } catch (OtpErlangExit | OtpErlangDecodeException e) {
                    System.err.println("Error receiving message: " + e.getMessage());
                    // Optionally, you can break the loop or take other actions here

                }
            }
        } catch (IOException e) {
            System.err.println("Error initializing OtpNode: " + e.getMessage());
        } finally {
            if (otpNode != null) {
                otpNode.close();
            }
            thPool.shutdown();
        }



    }
}

