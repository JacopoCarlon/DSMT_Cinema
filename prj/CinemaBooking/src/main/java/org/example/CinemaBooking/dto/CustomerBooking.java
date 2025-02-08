package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.*;

public class CustomerBooking {
    String customer;
    Long bookedSeats;

    public CustomerBooking(String customer, Long bookedSeats) {
        this.customer = customer;
        this.bookedSeats = bookedSeats;
    }

    public String getCustomer() {
        return customer;
    }

    public Long getBookedSeats() {
        return bookedSeats;
    }

    public void setBookedSeats(Long numSeats) {
        this.bookedSeats = numSeats;
    }

    @Override
    public String toString() {
        return "CustomerBooking{customer: " + customer + ", numSeats: " + bookedSeats + "}";
    }

    public OtpErlangMap toOtpErlangMap() {
        return new OtpErlangMap(
                new OtpErlangObject[]{ new OtpErlangString("username"), new OtpErlangString("num_seats")},
                new OtpErlangObject[]{ new OtpErlangString(customer), new OtpErlangLong(bookedSeats) }
        );
    }

    public static CustomerBooking decodeFromOtpErlangTuple(OtpErlangTuple tuple) {
        String customer = ((OtpErlangString) tuple.elementAt(0)).stringValue();
        Long   numSeats = ((OtpErlangLong) tuple.elementAt(1)).longValue();

        return new CustomerBooking(customer, numSeats);
    }
}
