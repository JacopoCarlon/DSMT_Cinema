package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.*;

import java.util.ArrayList;

public class Cinema {
    long            cinemaID;
    String          cinemaName;
    String          cinemaPassword;
    String          cinemaLocation;

    public Cinema(long cinemaID, String cinemaName, String cinemaPassword, String cinemaLocation){
        this.cinemaID = cinemaID;
        this.cinemaName = cinemaName;
        this.cinemaPassword = cinemaPassword;
        this.cinemaLocation = cinemaLocation;
    }

    public long getCinemaID() {
        return cinemaID;
    }

    public String getCinemaName() {
        return cinemaName;
    }

    public String getCinemaLocation() {
        return cinemaLocation;
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

        return new Cinema(cinemaID, cinemaName, null, cinemaLocation);
    }



    @Override
    public String toString() {
        return  "Cinema{cinemaName=" + cinemaName  +
                ", cinemaLocation=" + cinemaLocation + "}\n";
    }
}
