package org.example.CinemaBooking.dto;
import java.util.ArrayList;

public class ShowList {

    ArrayList<Show> showsList;
    boolean active;

    public ShowList(ArrayList<Show> new_showList, boolean active) {
        this.showsList = new_showList;
        this.active = active;
    }

    public ArrayList<Show> getShowsList() {
        return showsList;
    }

    public boolean isActive() {
        return active;
    }
}
