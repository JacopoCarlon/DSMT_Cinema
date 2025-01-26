package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangMap;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import java.util.ArrayList;
import java.util.Date;
import java.util.Objects;

public class Customer {
    String              username;
    String              password;
    ArrayList<Booking>  userBookingList;


    public Customer(String username, String password, ArrayList<Booking> usrBookings) {
        this.username = username;
        this.password = password;
        this.userBookingList = usrBookings;
    }

    public String getUsername() {
        return username;
    }

    public String getPassword() {
        return password;
    }

    public ArrayList<Booking> getUserBookingList() {
        return this.userBookingList;
    }

    public void changeBookingsForShow(String trgShowName, String cinemaName, Date showDate, long newSeatsCount){
        for (Booking booking : userBookingList) {
            if (Objects.equals(booking.showName, trgShowName)
                    && Objects.equals(booking.cinemaName, cinemaName)
                    && booking.showDate == showDate){
                if (newSeatsCount==0){
                    userBookingList.remove(booking);
                }else{
                    booking.num_seats = newSeatsCount;
                }
                break;
            }
        }
    }

    public boolean addNewBookingForShow(String trgShowName, String cinemaName, Date showDate , long newSeatsCount){
        return userBookingList.add(new Booking(username, trgShowName, cinemaName, showDate, newSeatsCount));
    }


    @Override
    public String toString() {
        return  "Customer{username=" + username  +
                ", password=" + password +
                ", userBookings=" + this.userBookingList.toString() + "}\n";
    }

    public OtpErlangMap toOtpErlangMap() {
        return new OtpErlangMap(
                new OtpErlangObject[]{new OtpErlangAtom("username"), new OtpErlangString("password")},
                new OtpErlangObject[]{new OtpErlangAtom(username), new OtpErlangString(password)}
        );
    }
}
