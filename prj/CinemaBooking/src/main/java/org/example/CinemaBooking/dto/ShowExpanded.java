package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.*;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

// Used  by customer for their own booking
public class ShowExpanded extends Show {
    Long committedBooking;
    Long waitingForCommitBooking;

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
            Long committedBooking,
            Long waitingForCommitBooking
    ) {
        super(showID, showName, showDate, cinemaID, cinemaName, cinemaLocation, maxSeats, currAvailableSeats, isEnded);
        this.committedBooking = committedBooking;
        this.waitingForCommitBooking = waitingForCommitBooking;
    }

    public ShowExpanded(Show show, Long committedBooking, Long waitingForCommitBooking) {
        super(show);
        this.committedBooking = committedBooking;
        this.waitingForCommitBooking = waitingForCommitBooking;
    }

    public Long getCommittedBooking() {
        return committedBooking;
    }

    public Long getWaitingForCommitBooking() {
        return waitingForCommitBooking;
    }

    public static ShowExpanded decodeFromErlangList(OtpErlangList list){
        Show base = Show.decodeFromErlangList(list);
        Long committedBooking = ((OtpErlangLong) list.elementAt(9)).longValue();

        Long waitingForCommitBooking = null;
        if (list.elementAt(10) != null)
            waitingForCommitBooking = ((OtpErlangLong) list.elementAt(10)).longValue();

        return new ShowExpanded(base, committedBooking, waitingForCommitBooking);
    }
}
