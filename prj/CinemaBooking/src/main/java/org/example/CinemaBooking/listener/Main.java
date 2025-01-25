package org.example.CinemaBooking.listener;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Main {
    private static final String node_name = "listener@localhost";
    private static final String cookie = "abcde";
    private static final String mailbox = "E_mailbox";
    private static final int THREAD_POOL_SIZE = 5;

    public static void main(String[] args) throws IOException, OtpErlangDecodeException, OtpErlangExit {
        // i tried to try catch this, cannot find a nice way to do this tbh
        ExecutorService thPool = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
        OtpNode otpNode = new OtpNode(node_name, cookie);

        // exchange messages with erlang :
        // https://www.erlang.org/docs/27/apps/jinterface/assets/java/com/ericsson/otp/erlang/otpmbox
        OtpMbox otpMbox = otpNode.createMbox(mailbox);

        while (true) {
            System.out.println("Receiving Erlang msg ...");
            // receive can call exeptions
            OtpErlangObject message = otpMbox.receive();
            Runnable r = new ErlangMessageTask(message);
            thPool.execute(r);
        }

    }
}

