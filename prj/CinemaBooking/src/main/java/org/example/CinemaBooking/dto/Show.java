package org.example.CinemaBooking.dto;

import java.util.Date;

import com.ericsson.otp.erlang.*;

// Used when booked seats are not relevant
public class Show {
    Long    showID;
    String  showName;
    String  showDate;
    Long    cinemaID;
    String  cinemaName;
    String  cinemaLocation;
    Long    maxSeats;
    Long    currAvailableSeats;
    Boolean isEnded;

    public Show(
            Long showID,
            String showName,
            String showDate,
            Long cinemaID,
            String cinemaName,
            String cinemaLocation,
            Long maxSeats,
            Long currAvailableSeats,
            Boolean isEnded)
    {
        this.showID = showID;
        this.showName = showName;
        this.showDate = showDate;
        this.cinemaID = cinemaID;
        this.cinemaName = cinemaName;
        this.cinemaLocation = cinemaLocation;
        this.maxSeats = maxSeats;
        this.currAvailableSeats = currAvailableSeats;
        this.isEnded = isEnded;
    }

    public Show(
            Long showID,
            String showName,
            String showDate,
            Cinema cinema,
            Long maxSeats,
            Long currAvailableSeats,
            Boolean isEnded
    ) {
        this.showID = showID;
        this.showName = showName;
        this.showDate = showDate;
        this.maxSeats = maxSeats;
        this.currAvailableSeats = currAvailableSeats;
        this.isEnded = isEnded;
        this.cinemaID = cinema.getCinemaID();
        this.cinemaName = cinema.getCinemaName();
        this.cinemaLocation = cinema.getCinemaLocation();
    }

    public Show(Show otherShow) {
        this.showID = otherShow.showID;
        this.showName = otherShow.showName;
        this.showDate = otherShow.showDate;
        this.cinemaID = otherShow.cinemaID;
        this.cinemaName = otherShow.cinemaName;
        this.cinemaLocation = otherShow.cinemaLocation;
        this.maxSeats = otherShow.maxSeats;
        this.currAvailableSeats = otherShow.currAvailableSeats;
        this.isEnded = otherShow.isEnded;
    }

    public Show(
            String showName,
            String showDate,
            Long cinemaID,
            Long maxSeats
    ){
        this(0L, showName ,showDate, cinemaID, null, null, maxSeats, null,false);
    }

    public Long getShowID(){
        return this.showID;
    }

    public String getShowName(){
        return this.showName;
    }

    public String getShowDate() {
        return this.showDate;
    }

    public Long getCinemaID(){return this.cinemaID;}

    public String getCinemaName(){return this.cinemaName;}

    public String getCinemaLocation(){return this.cinemaLocation;}

    public Long getMaxSeats() { return this.maxSeats; }

    public Long getCurrAvailableSeats() { return this.currAvailableSeats; }

    public boolean getIsEnded() { return this.isEnded; }

    @Override
    public String toString(){
        return "Show{showID: " + showID.toString() +
                ", showName: " + showName +
                ", showDate: " + showDate +
                ", cinemaID: " + cinemaID.toString() +
                ", cinemaName: " + cinemaName +
                ", cinemaLocation: " + cinemaLocation +
                ", maxSeats: " + maxSeats.toString() +
                ", currAvailableSeats: " + currAvailableSeats.toString() +
                ", isEnded: " + isEnded.toString() + "}\n";
    }

    // todo
    public OtpErlangMap toOtpErlangMap(){
        // Map only necessary info
        return new OtpErlangMap(
            new OtpErlangObject[]{
                    new OtpErlangString("show_id"),
                    new OtpErlangString("show_name"),
                    new OtpErlangString("show_date"),
                    new OtpErlangString("cinema_id"),
                    new OtpErlangString("max_seats")
            },
            new OtpErlangObject[]{
                    new OtpErlangLong(showID),
                    new OtpErlangString(showName),
                    new OtpErlangString(showDate),
                    new OtpErlangLong(cinemaID),
                    new OtpErlangLong(maxSeats)
            }
        );
    }

    public OtpErlangMap showIDtoOtpErlangMap(){
        return new OtpErlangMap(
                new OtpErlangObject[]{new OtpErlangString("showID") },
                new OtpErlangObject[]{new OtpErlangLong(showID)}
        );
    }

    // todo
    public static Show decodeFromErlangList(OtpErlangList list){
        long    showID   = ((OtpErlangLong) list.elementAt(0)).longValue();
        String  showName = ((OtpErlangString) list.elementAt(1)).stringValue();
        String  showDate = ((OtpErlangString) list.elementAt(2)).stringValue();
        long    cinemaID   = ((OtpErlangLong) list.elementAt(3)).longValue();
        String  cinemaName = ((OtpErlangString) list.elementAt(4)).stringValue();
        String  cinemaLocation = ((OtpErlangString) list.elementAt(5)).stringValue();
        long    maxSeats = ((OtpErlangLong) list.elementAt(6)).longValue();
        long    currAvailableSeats = ((OtpErlangLong) list.elementAt(7)).longValue();
        Boolean isEnded = ((OtpErlangBoolean) list.elementAt(8)).booleanValue();

        return new Show(showID, showName, showDate, cinemaID, cinemaName, cinemaLocation, maxSeats, currAvailableSeats, isEnded);
    }

}
