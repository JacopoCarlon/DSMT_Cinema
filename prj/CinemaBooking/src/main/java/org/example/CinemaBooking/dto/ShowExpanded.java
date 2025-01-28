package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangString;

public class ShowExpanded {
    String  showID;
    String  showName;
    String  showDate;
    long    maxSeats;
    long    currAvailableSeats;
    Boolean isEnded;
    String username;
    String cinemaName;
    String cinemaLocation;
    long numSeats;
    String is_a_cinema;

    public ShowExpanded(
            String  showID,
            String  showName,
            String  showDate,
            long    maxSeats,
            long    currAvailableSeats,
            Boolean isEnded,
            String cinemaName,
            String cinemaLocation,
            String username,
            long numSeats,
            String is_a_cinema

            ){

        this.showID = showID ;
        this.showName = showName ;
        this.showDate = showDate ;
        this.maxSeats =  maxSeats;
        this.currAvailableSeats = currAvailableSeats ;
        this.isEnded = isEnded ;
        this.cinemaName = cinemaName ;
        this.cinemaLocation = cinemaLocation ;
        this.username = username ;
        this.numSeats = numSeats ;
        this.is_a_cinema = is_a_cinema ;
    }

    public String getShowID() {
        return showID;
    }

    public String getShowName() {
        return showName;
    }

    public String getShowDate() {
        return showDate;
    }

    public long getMaxSeats() {
        return maxSeats;
    }

    public long getCurrAvailableSeats() {
        return currAvailableSeats;
    }

    public Boolean getEnded() {
        return isEnded;
    }

    public String getCinemaName() {
        return cinemaName;
    }

    public String getCinemaLocation() {
        return cinemaLocation;
    }

    public String getUsername() {
        return username;
    }

    public long getNumSeats() {
        return numSeats;
    }

    public String getIs_a_cinema() {
        return is_a_cinema;
    }

/*
this.showID = showID ;
this.showName = showName ;
this.showDate = showDate ;
this.maxSeats =  maxSeats;
this.currAvailableSeats = currAvailableSeats ;
this.isEnded = isEnded ;
this.cinemaName = cinemaName ;
this.cinemaLocation = cinemaLocation ;
this.username = username ;
this.numSeats = numSeats ;
this.is_a_cinema = is_a_cinema ;
*/
    public static ShowExpanded decodeFromErlangList(OtpErlangList list) {
        String  showID   = ((OtpErlangString) list.elementAt(0)).stringValue();
        String  showName = ((OtpErlangString) list.elementAt(1)).stringValue();
        String  showDate = ((OtpErlangString) list.elementAt(2)).stringValue();
        long    maxSeats = ((OtpErlangLong) list.elementAt(3)).longValue();
        long    currAvailableSeats = ((OtpErlangLong) list.elementAt(4)).longValue();
        Boolean isEnded = ((OtpErlangBoolean) list.elementAt(5)).booleanValue();
        String  cinemaName = ((OtpErlangString) list.elementAt(6)).stringValue();
        String  cinemaLocation = ((OtpErlangString) list.elementAt(7)).stringValue();
        String  username = ((OtpErlangString) list.elementAt(8)).stringValue();
        long    numSeats = ((OtpErlangLong) list.elementAt(9)).longValue();
        String  is_a_cinema = ((OtpErlangString) list.elementAt(10)).stringValue();

        return new ShowExpanded(showID, showName, showDate, maxSeats, currAvailableSeats, isEnded, cinemaName, cinemaLocation, username, numSeats, is_a_cinema);
    }

    public String toString(){
        return "Show{showID: " + showID +
                ", showName: " + showName +
                ", showDate: " + showDate +
                ", maxSeats: " + maxSeats +
                ", currAvailableSeats: " + currAvailableSeats +
                ", isEnded: " + isEnded +
                ", cinemaName: " + cinemaName +
                ", cinemaLocation: " + cinemaLocation +
                ", username: " + username +
                ", numSeats: " + numSeats +
                ", is_a_cinema: " + is_a_cinema +
                "}\n";
    }
}
