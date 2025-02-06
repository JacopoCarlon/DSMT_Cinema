package org.example.CinemaBooking.dto;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import java.util.*;
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
        super(showID, showName, showDate, cinemaID, cinemaName, cinemaLocation, maxSeats, currAvailableSeats, isEnded);
        this.committedBookingsList = committedBookingsList;
        this.waitingForCommitList = waitingForCommitList;
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
        super(showID, showName, showDate, cinema, maxSeats, currAvailableSeats, isEnded);
        this.committedBookingsList = committedBookingsList;
        this.waitingForCommitList = waitingForCommitList;
    }

    public ShowWithBookings(
            Show otherShow,
            List<CustomerBooking> committedBookingsList,
            List<CustomerBooking> waitingForCommitList
    ) {
        super(otherShow);
        this.committedBookingsList = committedBookingsList;
        this.waitingForCommitList = waitingForCommitList;
    }

    //////////////////////////////////////////////////////////////////////////////
    public List<CustomerBooking> getCommittedBookingsList() {
        return committedBookingsList;
    }

    public Long getFirstCommittedBooking() {
        if (committedBookingsList == null || committedBookingsList.isEmpty())
            return 0L;
        return committedBookingsList.getFirst().bookedSeats;
    }

    public List<CustomerBooking> getWaitingForCommitList() {
        return waitingForCommitList;
    }

    public Long getFirstWaitingBooking() {
        if (waitingForCommitList == null || waitingForCommitList.isEmpty())
            return 0L;
        return waitingForCommitList.getFirst().bookedSeats;
    }

    /////////////////////////////////////////////////////////////////////////////////////
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


    public class Triple{
        String username;
        Long storedBooking;
        Long waitingBooking;

        public Triple(String uName, Long storedBooking, Long waitingBooking){
            this.username = uName;
            this.storedBooking = storedBooking;
            this.waitingBooking = waitingBooking;
        }

        public String getUsername() {
            return username;
        }

        public Long getStoredBooking() {
            return storedBooking;
        }

        public Long getWaitingBooking() {
            return waitingBooking;
        }
    }


    public List<Triple> getFullOuterJoinBookings(){
        // // List<CustomerBooking> committedBookingsList = this.committedBookingsList;
        // // List<CustomerBooking> waitingForCommitList = this.waitingForCommitList;
        // Map to store the merged results
        Map<String, Triple> map = new HashMap<>();
        // Process committedBookingsList
        for (CustomerBooking cBooking : this.committedBookingsList) {
            map.put(cBooking.customer, new Triple(cBooking.customer, cBooking.bookedSeats, cBooking.bookedSeats));
        }
        // Process waitingForCommitList
        for (CustomerBooking wBooking : this.waitingForCommitList) {
            if (map.containsKey(wBooking.customer)) {
                // If the username exists in the map, update the waitingBooking
                Triple this_triple = map.get(wBooking.customer);
                this_triple.waitingBooking = wBooking.bookedSeats;
            } else {
                // If the username does not exist in the map, add a new Triple with storedBooking as 0
                map.put(wBooking.customer, new Triple(wBooking.customer, 0L, wBooking.bookedSeats));
            }
        }
        // Convert the map to a List<Triple>
        return new ArrayList<>(map.values());
    }



}
