package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.*;

import java.util.ArrayList;
import java.util.Date;

public class Cinema {
    String          cinemaID;
    String          cinemaName;
    String          cinemaPassword;
    String          cinemaLocation;
    ArrayList<Show> cinemaShowList;

    public Cinema(String cinemaID, String cinemaName, String cinemaPassword, String cinemaLocation, ArrayList<Show> showList){
        this.cinemaID = cinemaID;
        this.cinemaName = cinemaName;
        this.cinemaPassword = cinemaPassword;
        this.cinemaLocation = cinemaLocation;
        this.cinemaShowList = showList;
    }

    public ArrayList<Show> getCinemaShowList(){
        return this.cinemaShowList;
    }

    public Boolean addShow( String showID,
                            String showName,
                            String showDate,
                            long maxSeats,
                            long currAvailableSeats,
                            boolean isEnded) {
        return this.cinemaShowList.add(new Show(showID, showName, showDate, maxSeats, currAvailableSeats, isEnded));
    }

    public String getCinemaID(){
        return this.cinemaID;
    }

    public OtpErlangMap toOtpErlangMapNoShows() {
        return new OtpErlangMap(
                new OtpErlangObject[]{new OtpErlangString("cinemaID"),new OtpErlangString("cinemaName"), new OtpErlangString("cinemaPassword"), new OtpErlangString("cinemaLocation")},
                new OtpErlangObject[]{new OtpErlangString(cinemaID), new OtpErlangString(cinemaName), new OtpErlangString(cinemaPassword), new OtpErlangString(cinemaLocation)}
        );
    }


    public OtpErlangMap cinemaNamePwdToOtpErlangMapNoShows() {
        return new OtpErlangMap(
                new OtpErlangObject[]{new OtpErlangString("cinemaName"), new OtpErlangString("cinemaPassword")},
                new OtpErlangObject[]{new OtpErlangString(cinemaName), new OtpErlangString(cinemaPassword)}
        );
    }


    public static Cinema decodeFromErlangList(OtpErlangList list){
        String          cinemaID        = ((OtpErlangString) list.elementAt(0)).stringValue();
        String          cinemaName      = ((OtpErlangString) list.elementAt(1)).stringValue();
        String          cinemaPassword  = ((OtpErlangString) list.elementAt(2)).stringValue();
        String          cinemaLocation  = ((OtpErlangString) list.elementAt(3)).stringValue();
        ArrayList<Show> cinemaShowList  = null;

        return new Cinema(cinemaID, cinemaName, cinemaPassword, cinemaLocation, cinemaShowList);
    }



    @Override
    public String toString() {
        return  "Cinema{cinemaName=" + cinemaName  +
                ", cinemaLocation=" + cinemaLocation +
                ", showList=" + cinemaShowList.toString() + "}\n";
    }
}
