package org.example.CinemaBooking.dto;

import java.util.ArrayList;
import java.util.Date;

public class Cinema {
    String          cinemaName;
    String          cinemaLocation;
    ArrayList<Show> cinemaShowList;

    public Cinema(String cinemaName, String cinemaLocation, ArrayList<Show> showList){
        this.cinemaName = cinemaName;
        this.cinemaLocation = cinemaLocation;
        this.cinemaShowList = showList;
    }

    public ArrayList<Show> getCinemaShowList(){
        return this.cinemaShowList;
    }

    public Boolean addShow(String showName,
                           Date showDate,
                           long maxSeats,
                           long currAvailableSeats,
                           boolean isEnded) {
        return this.cinemaShowList.add(new Show(showName, showDate, maxSeats, currAvailableSeats, isEnded));
    }

    @Override
    public String toString() {
        return  "Cinema{cinemaName=" + cinemaName  +
                ", cinemaLocation=" + cinemaLocation +
                ", showList=" + cinemaShowList.toString() + "}\n";
    }
}
