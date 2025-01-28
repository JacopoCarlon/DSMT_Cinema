package org.example.CinemaBooking.dto;
import java.util.ArrayList;

public class ShowList {

    ArrayList<Show> showsList;

    public ShowList(ArrayList<Show> new_showList) {
        this.showsList = new_showList;
    }

    public ArrayList<Show> getShowsList() {
        return showsList;
    }

}
