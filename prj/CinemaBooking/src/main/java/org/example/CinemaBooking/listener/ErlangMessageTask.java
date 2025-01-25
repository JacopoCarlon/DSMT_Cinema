package org.example.CinemaBooking.listener;

import com.ericsson.otp.erlang.*;



public class ErlangMessageTask implements Runnable {
    private OtpErlangObject message;


    public ErlangMessageTask(OtpErlangObject inputErlangMsg) {
        this.message = inputErlangMsg;
    }

    @Override
    public void run() {
        if(message instanceof OtpErlangTuple){
            //{self(), destinationAtom, {ResultTuple}}
            OtpErlangTuple resultTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(2);
            OtpErlangAtom destination_atom = (OtpErlangAtom) ((OtpErlangTuple) message).elementAt(1);

            //TODO : do actions based on db response

        }
    }



}

