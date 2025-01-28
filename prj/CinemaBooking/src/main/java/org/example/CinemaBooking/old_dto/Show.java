package org.example.CinemaBooking.old_dto;

import com.ericsson.otp.erlang.*;


// This class is only seen in the Cinema Page
public class Show {
    long    showID;
    String  showName;
    String  showDate;
    long    maxSeats;
    long    currAvailableSeats;
    Boolean isEnded;

    public Show(
            long showID,
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
            long showID,
            String showName,
            String showDate,
            long maxSeats
    ){
        this(showID, showName ,showDate,maxSeats,maxSeats,false);
    }

    public Show(
            String showName,
            String showDate,
            long maxSeats,
            long currAvailableSeats){
        this(0, showName, showDate, maxSeats, currAvailableSeats, false);
    }

    public Show(long showID, String showName, String showDate, long maxSeats, long currAvailableSeats) {
        this(showID, showName, showDate, maxSeats, currAvailableSeats, false);
    }

    public long getShowID(){
        return this.showID;
    }

    public String getShowName(){
        return this.showName;
    }

    public String getShowDate() {
        return this.showDate;
    }

    public long getMaxSeats() { return this.maxSeats; }

    public long getCurrAvailableSeats() { return this.currAvailableSeats; }

    public boolean getIsEnded() { return this.isEnded; }

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
            new OtpErlangObject[]{new OtpErlangLong(showID), new OtpErlangString(showName), new OtpErlangString(showDate), new OtpErlangLong(maxSeats), new OtpErlangLong(currAvailableSeats), new OtpErlangBoolean(isEnded)}
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
        long    maxSeats = ((OtpErlangLong) list.elementAt(3)).longValue();
        long    currAvailableSeats = ((OtpErlangLong) list.elementAt(4)).longValue();
        Boolean isEnded = ((OtpErlangBoolean) list.elementAt(5)).booleanValue();

        return new Show(showID, showName, showDate, maxSeats, currAvailableSeats, isEnded);
    }

}
