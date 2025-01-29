package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.*;

public class CustomerBooking {
    String customer;
    Long bookingSeats;

    public CustomerBooking(String customer, Long bookingSeats) {
        this.customer = customer;
        this.bookingSeats = bookingSeats;
    }

    public String getCustomer() {
        return customer;
    }

    public Long getBookingSeats() {
        return bookingSeats;
    }

    public void setBookingSeats(Long numSeats) {
        this.bookingSeats = numSeats;
    }

    @Override
    public String toString() {
        return "CustomerBooking{customer: " + customer + ", numSeats: " + bookingSeats + "}";
    }

    public OtpErlangMap toOtpErlangMap() {
        return new OtpErlangMap(
                new OtpErlangObject[]{ new OtpErlangString("customer"), new OtpErlangString("num_seats")},
                new OtpErlangObject[]{ new OtpErlangString(customer), new OtpErlangLong(bookingSeats) }
        );
    }

    public static CustomerBooking decodeFromOtpErlangTuple(OtpErlangTuple tuple) {
        String customer = ((OtpErlangString) tuple.elementAt(0)).stringValue();
        Long   numSeats = ((OtpErlangLong) tuple.elementAt(1)).longValue();

        return new CustomerBooking(customer, numSeats);
    }
}
