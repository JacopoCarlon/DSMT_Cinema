package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class ShowWithBookings extends Show {
    List<CustomerBooking> committedBookingsList;
    List<CustomerBooking> waitingForCommitList;

    public ShowWithBookings(
            Long showID,
            String showName,
            String showDate,
            Long cinemaID,
            String cinemaName,
            String cinemaLocation,
            Long maxSeats,
            Long currAvailableSeats,
            Boolean isEnded,
            List<CustomerBooking> committedBookingsList,
            List<CustomerBooking> waitingForCommitList
    ) {
        this.committedBookingsList = committedBookingsList;
        this.waitingForCommitList = waitingForCommitList;
        super(showID, showName, showDate, cinemaID, cinemaName, cinemaLocation, maxSeats, currAvailableSeats, isEnded);
    }

    public ShowWithBookings(
            Long showID,
            String showName,
            String showDate,
            Cinema cinema,
            Long maxSeats,
            Long currAvailableSeats,
            Boolean isEnded,
            List<CustomerBooking> committedBookingsList,
            List<CustomerBooking> waitingForCommitList
    ){
        this.committedBookingsList = committedBookingsList;
        this.waitingForCommitList = waitingForCommitList;
        super(showID, showName, showDate, cinema, maxSeats, currAvailableSeats, isEnded);
    }

    public ShowWithBookings(
            Show otherShow,
            List<CustomerBooking> committedBookingsList,
            List<CustomerBooking> waitingForCommitList
    ) {
        this.committedBookingsList = committedBookingsList;
        this.waitingForCommitList = waitingForCommitList;
        super(otherShow);
    }

    public static ShowWithBookings decodeFromErlangList(OtpErlangList list) {
        Show baseShow = Show.decodeFromErlangList(list);

        List<CustomerBooking> committedBookingsList = null;
        if (list.elementAt(9) != null) {
            OtpErlangObject[] committedTupleList = ((OtpErlangList) list.elementAt(9)).elements();
            committedBookingsList = Arrays.stream(committedTupleList)
                    .map(tuple -> CustomerBooking.decodeFromOtpErlangTuple((OtpErlangTuple) tuple))
                    .toList();
        }

        List<CustomerBooking> waitingForCommitList = null;
        if (list.elementAt(9) != null) {
            OtpErlangObject[] waitingTupleList = ((OtpErlangList) list.elementAt(9)).elements();
            waitingForCommitList = Arrays.stream(waitingTupleList)
                    .map(tuple -> CustomerBooking.decodeFromOtpErlangTuple((OtpErlangTuple) tuple))
                    .toList();
        }

        return new ShowWithBookings(baseShow, committedBookingsList, waitingForCommitList);
    }
}
