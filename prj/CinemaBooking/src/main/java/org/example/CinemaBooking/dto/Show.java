package org.example.CinemaBooking.dto;

import java.util.Date;

import com.ericsson.otp.erlang.OtpErlangMap;


// This class is only seen in the Cinema Page
public class Show {
    String  showName;
    Date    showDate;
    long    maxSeats;
    long    currAvailableSeats;
    Boolean isEnded;

    public Show(
            String showName,
            Date showDate,
            long maxSeats ,
            long currAvailableSeats,
            Boolean isEnded)
    {
        this.showName = showName;
        this.showDate = showDate;
        this.maxSeats = maxSeats;
        this.currAvailableSeats = currAvailableSeats;
        this.isEnded = isEnded;
    }

    public Show(
            String showName,
            Date showDate,
            long maxSeats
    ){
        this(showName,showDate,maxSeats,0,false);
    }

    public String getShowName(){
        return this.showName;
    }

    public Date getShowDate() {
        return showDate;
    }

    @Override
    public String toString(){
        return "Show{showName: " + showName +
                ", showDate: " + showDate +
                ", maxSeats: " + maxSeats +
                ", currAvailableSeats: " + currAvailableSeats +
                ", isEnded: " + isEnded + "}\n";
    }

    // todo
    //  public OtpErlangMap encodeInErlangMap(){
    //
    //  }

    // todo
    //  public OtpErlangMap decodeFromErlangMap(){
    //
    //  }

}
