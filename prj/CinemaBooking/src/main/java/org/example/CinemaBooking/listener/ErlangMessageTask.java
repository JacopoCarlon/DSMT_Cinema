package org.example.CinemaBooking.listener;

import com.ericsson.otp.erlang.*;



public class ErlangMessageTask implements Runnable {
    private OtpErlangObject message;

    // todo : test something
    private static final String base_uri = "localhost:8080/CinemaBooking";


    public ErlangMessageTask(OtpErlangObject inputErlangMsg) {
        this.message = inputErlangMsg;
    }

    @Override
    public void run() {
        if(message instanceof OtpErlangTuple){
            //{self(), destinationAtom, {ResultTuple}}
            OtpErlangAtom destination_atom = (OtpErlangAtom) ((OtpErlangTuple) message).elementAt(1);
            OtpErlangTuple resultTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(2);

            //TODO : do actions based on db response
            // possibly stop bookings for old show ??

        }
    }



}

