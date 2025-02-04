package org.example.CinemaBooking.dto;
import java.util.List;

public class ShowWithBookingsList {

    List<ShowWithBookings> showsWithBookingsList;

    public ShowWithBookingsList(List<ShowWithBookings> new_showWithBookingsList) {
        this.showsWithBookingsList = new_showWithBookingsList;
    }

    public List<ShowWithBookings> getShowsList() {
        return showsWithBookingsList;
    }

}
