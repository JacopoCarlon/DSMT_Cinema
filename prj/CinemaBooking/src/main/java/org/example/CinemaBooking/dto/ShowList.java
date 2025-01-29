package org.example.CinemaBooking.dto;
import java.util.List;

public class ShowList {

    List<ShowExpanded> showsList;

    public ShowList(List<ShowExpanded> new_showList) {
        this.showsList = new_showList;
    }

    public List<ShowExpanded> getShowsList() {
        return showsList;
    }

}
