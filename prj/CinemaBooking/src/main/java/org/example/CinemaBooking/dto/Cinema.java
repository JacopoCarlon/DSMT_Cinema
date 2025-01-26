package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

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


    @Override
    public String toString() {
        return  "Cinema{cinemaName=" + cinemaName  +
                ", cinemaLocation=" + cinemaLocation +
                ", showList=" + cinemaShowList.toString() + "}\n";
    }
}
