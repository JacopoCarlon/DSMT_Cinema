package org.example.CinemaBooking.old_dto;

import java.util.ArrayList;

public class BookingList {
    ArrayList<Booking> bookingsList;
    boolean active;

    public BookingList(ArrayList<Booking> new_bookingsList, boolean active) {
        this.bookingsList = new_bookingsList;
        this.active = active;
    }

    public ArrayList<Booking> getBookingsList() {
        return bookingsList;
    }

    public boolean isActive() {
        return active;
    }
}
