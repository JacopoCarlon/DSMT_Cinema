package org.example.CinemaBooking.dto;

import java.util.Date;

import com.ericsson.otp.erlang.*;


// This class is only seen in the Cinema Page
public class Show {
    String  showID;
    String  showName;
    String  showDate;
    long    maxSeats;
    long    currAvailableSeats;
    Boolean isEnded;

    public Show(
            String showID,
            String showName,
            String showDate,
            long maxSeats ,
            long currAvailableSeats,
            Boolean isEnded)
    {
        this.showID = showID;
        this.showName = showName;
        this.showDate = showDate;
        this.maxSeats = maxSeats;
        this.currAvailableSeats = currAvailableSeats;
        this.isEnded = isEnded;
    }

    public Show(
            String showID,
            String showName,
            String showDate,
            long maxSeats
    ){
        this(showID, showName ,showDate,maxSeats,0,false);
    }

    public String getShowID(){
        return this.showID;
    }

    public String getShowName(){
        return this.showName;
    }

    public String getShowDate() {
        return showDate;
    }

    @Override
    public String toString(){
        return "Show{showID: " + showID +
                ", showName: " + showName +
                ", showDate: " + showDate +
                ", maxSeats: " + maxSeats +
                ", currAvailableSeats: " + currAvailableSeats +
                ", isEnded: " + isEnded + "}\n";
    }

    // todo
    public OtpErlangMap toOtpErlangMap(){
        return new OtpErlangMap(
            new OtpErlangObject[]{new OtpErlangString("show_id"), new OtpErlangString("show_name"), new OtpErlangString("show_date"), new OtpErlangString("max_seats"), new OtpErlangString("currAvailableSeats"), new OtpErlangString("isEnded") },
            new OtpErlangObject[]{new OtpErlangString(showID), new OtpErlangString(showName), new OtpErlangString(showDate), new OtpErlangLong(maxSeats), new OtpErlangLong(currAvailableSeats), new OtpErlangBoolean(isEnded)}
        );
    }

    public OtpErlangMap showIDtoOtpErlangMap(){
        return new OtpErlangMap(
                new OtpErlangObject[]{new OtpErlangString("showID") },
                new OtpErlangObject[]{new OtpErlangString(showID)}
        );
    }

    // todo
    public static Show decodeFromErlangList(OtpErlangList list){
        String  showID   = ((OtpErlangString) list.elementAt(0)).stringValue();
        String  showName = ((OtpErlangString) list.elementAt(1)).stringValue();
        String  showDate = ((OtpErlangString) list.elementAt(2)).stringValue();
        long    maxSeats = ((OtpErlangLong) list.elementAt(3)).longValue();
        long    currAvailableSeats = ((OtpErlangLong) list.elementAt(4)).longValue();
        Boolean isEnded = ((OtpErlangBoolean) list.elementAt(5)).booleanValue();

        return new Show(showID, showName, showDate, maxSeats, currAvailableSeats, isEnded);
    }

}
