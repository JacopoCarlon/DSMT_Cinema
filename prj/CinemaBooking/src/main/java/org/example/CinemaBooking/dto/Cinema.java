package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.*;

import java.util.ArrayList;

public class Cinema {
    long            cinemaID;
    String          cinemaName;
    String          cinemaPassword;
    String          cinemaLocation;

    public Cinema(long cinemaID, String cinemaName, String cinemaPassword, String cinemaLocation, ArrayList<Show> showList){
        this.cinemaID = cinemaID;
        this.cinemaName = cinemaName;
        this.cinemaPassword = cinemaPassword;
        this.cinemaLocation = cinemaLocation;
    }

    public OtpErlangMap toOtpErlangMap() {
        return new OtpErlangMap(
                new OtpErlangObject[]{
                        new OtpErlangString("cinema_iD"),
                        new OtpErlangString("cinema_name"),
                        new OtpErlangString("cinema_password"),
                        new OtpErlangString("cinema_location")
                },
                new OtpErlangObject[]{
                        new OtpErlangLong(cinemaID),
                        new OtpErlangString(cinemaName),
                        new OtpErlangString(cinemaPassword),
                        new OtpErlangString(cinemaLocation)
                }
        );
    }

    public static Cinema decodeFromErlangList(OtpErlangList list){
        long            cinemaID        = ((OtpErlangLong) list.elementAt(0)).longValue();
        String          cinemaName      = ((OtpErlangString) list.elementAt(1)).stringValue();
        String          cinemaLocation  = ((OtpErlangString) list.elementAt(2)).stringValue();

        return new Cinema(cinemaID, cinemaName, null, cinemaLocation, null);
    }



    @Override
    public String toString() {
        return  "Cinema{cinemaName=" + cinemaName  +
                ", cinemaLocation=" + cinemaLocation + "}\n";
    }
}
