package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class ShowExtended extends Show {
    List<CustomerBooking> bookingList;

    public ShowExtended(
            Long showID,
            String showName,
            String showDate,
            Long cinemaID,
            String cinemaName,
            String cinemaLocation,
            Long maxSeats,
            Long currAvailableSeats,
            Boolean isEnded,
            List<CustomerBooking> bookingList
    ) {
        super(showID, showName, showDate, cinemaID, cinemaName, cinemaLocation, maxSeats, currAvailableSeats, isEnded);
        this.bookingList = bookingList;
    }

    public ShowExtended(Show show, List<CustomerBooking> bookingList) {
        super(show);
        this.bookingList = bookingList;
    }

    public List<CustomerBooking> getBookingList() {
        return bookingList;
    }

    public static ShowExtended decodeFromErlangList(OtpErlangList list){
        Show base = Show.decodeFromErlangList(list);

        OtpErlangObject[] tupleList = ((OtpErlangList) list.elementAt(9)).elements();
        List<CustomerBooking> bookingList = Arrays.stream(tupleList)
                .map(tuple -> CustomerBooking.decodeFromOtpErlangTuple((OtpErlangTuple) tuple))
                .collect(Collectors.toList());

        return new ShowExtended(base, bookingList);
    }
}
