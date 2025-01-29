package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.*;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

// Used by cinemas for complete list of bookings, and by customer for their own booking
public class ShowExpanded extends Show {
    List<CustomerBooking> bookingList;

    public ShowExpanded(
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

    public ShowExpanded(Show show, List<CustomerBooking> bookingList) {
        super(show);
        this.bookingList = bookingList;
    }

    public List<CustomerBooking> getBookingList() {
        return bookingList;
    }

    public CustomerBooking getFirstBooking() {
        if (bookingList == null || bookingList.isEmpty()) {
            return null;
        }
        return bookingList.getFirst();
    }

    public static ShowExpanded decodeFromErlangList(OtpErlangList list){
        Show base = Show.decodeFromErlangList(list);

        OtpErlangObject[] tupleList = ((OtpErlangList) list.elementAt(9)).elements();
        List<CustomerBooking> bookingList = Arrays.stream(tupleList)
                .map(tuple -> CustomerBooking.decodeFromOtpErlangTuple((OtpErlangTuple) tuple))
                .collect(Collectors.toList());

        return new ShowExpanded(base, bookingList);
    }
}
