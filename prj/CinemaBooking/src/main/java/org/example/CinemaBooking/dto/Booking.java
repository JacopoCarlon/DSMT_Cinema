package org.example.CinemaBooking.dto;

import java.util.Date;


// this class is seen in the Costumer Page !!!
public class Booking {
    String  username;
    String  showName;
    String  cinemaName;
    Date    showDate;
    long    num_seats;

    public Booking(String username, String showName, String cinemaName, Date showDate, long num_seats){
        // todo : manage negative number of seats reasonably ..?
        if (num_seats <= 0){
            num_seats = 0;
        }
        this.username = username;
        this.showName = showName;
        this.cinemaName = cinemaName;
        this.showDate = showDate;
        this.num_seats = num_seats;
    }

    public String getUsername(){
        return this.username;
    }

    public String showName(){
        return this.showName;
    }

    public String cinemaName(){
        return this.cinemaName;
    }

    public Date showDate(){
        return this.showDate;
    }

    public long num_seats(){
        return this.num_seats;
    }

    public String toString(){
        return "Booking{username: " + username +
                ", showName: " + showName +
                ", cinemaName: " + cinemaName +
                ", showDate: " + showDate +
                ", seatsBookedByUser: " + num_seats + "}\n";
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

