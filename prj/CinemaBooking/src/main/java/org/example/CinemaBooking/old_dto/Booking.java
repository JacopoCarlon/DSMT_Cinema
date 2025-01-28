package org.example.CinemaBooking.old_dto;

import com.ericsson.otp.erlang.*;


// this class is seen in the Costumer Page !!!
public class Booking {
    String  username;
    long    showID;
    String  showName;
    String  cinemaName;
    String  showDate;
    long    num_seats;

    public Booking(String username, long showID, String showName, String cinemaName, String showDate, long num_seats){
        // todo : manage negative number of seats reasonably ..?
        if (num_seats <= 0){
            num_seats = 0;
        }
        this.username = username;
        this.showID = showID;
        this.showName = showName;
        this.cinemaName = cinemaName;
        this.showDate = showDate;
        this.num_seats = num_seats;
    }

    public void setBookingSeats(long new_num_sets){
        this.num_seats = new_num_sets;
    }

    public String getUsername(){
        return this.username;
    }

    public long getShowID(){return this.showID;}

    public String showName(){
        return this.showName;
    }

    public String cinemaName(){
        return this.cinemaName;
    }

    public String showDate(){
        return this.showDate;
    }

    public long num_seats(){
        return this.num_seats;
    }

    public String toString(){
        return "Booking{username: " + username +
                ", showID: " + showID +
                ", showName: " + showName +
                ", cinemaName: " + cinemaName +
                ", showDate: " + showDate +
                ", seatsBookedByUser: " + num_seats + "}\n";
    }

    public static Booking decodeFromErlangList(OtpErlangList list){
        String  username    = ((OtpErlangString) list.elementAt(0)).stringValue();
        long    showID      = ((OtpErlangLong) list.elementAt(1)).longValue();
        String  showName    = ((OtpErlangString) list.elementAt(2)).stringValue();
        String  cinemaName  = ((OtpErlangString) list.elementAt(3)).stringValue();
        String  showDate    = ((OtpErlangString) list.elementAt(4)).stringValue();
        long    num_seats   = ((OtpErlangLong) list.elementAt(5)).longValue();

        return new Booking(username, showID, showName , cinemaName, showDate, num_seats);
    }


    public OtpErlangMap toOtpErlangMap() {
        return new OtpErlangMap(
                new OtpErlangObject[]{new OtpErlangString("username"),new OtpErlangString("showID"), new OtpErlangString("showName"), new OtpErlangString("cinemaName"), new OtpErlangString("showDate"), new OtpErlangString("num_seats") },
                new OtpErlangObject[]{new OtpErlangString(username), new OtpErlangLong(showID), new OtpErlangString(showName), new OtpErlangString(cinemaName), new OtpErlangString(showDate), new OtpErlangLong(num_seats) }
        );
    }


    public void increaseBookingSeats(long to_add){
        this.num_seats += to_add;
    }

    public boolean decreaseBookingSeats(long to_remove){
        this.num_seats -= to_remove;
        if (to_remove > this.num_seats){
            return false;
        }else{
            this.num_seats -= to_remove;
            return true;
        }
    }

}

