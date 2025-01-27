package org.example.CinemaBooking.dto;

import java.util.ArrayList;

public class BookingList {
    ArrayList<Booking> bookingsList;
    boolean active;

    public BookingList(ArrayList<Booking> auctionList, boolean active) {
        this.bookingsList = auctionList;
        this.active = active;
    }

    public ArrayList<Booking> getAuctionList() {
        return bookingsList;
    }

    public boolean isActive() {
        return active;
    }
}
