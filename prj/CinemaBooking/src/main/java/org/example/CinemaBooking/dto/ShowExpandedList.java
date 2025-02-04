package org.example.CinemaBooking.dto;
import java.util.List;

public class ShowExpandedList {

    List<ShowExpanded> showsList;

    public ShowExpandedList(List<ShowExpanded> new_showList) {
        this.showsList = new_showList;
    }

    public List<ShowExpanded> getShowsList() {
        return showsList;
    }

}
