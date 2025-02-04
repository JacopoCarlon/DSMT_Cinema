package org.example.CinemaBooking.websocket;

import com.google.gson.Gson;

import jakarta.websocket.*;
import org.example.CinemaBooking.dto.ShowWithBookings;

public class ShowWithBookingsListDecoder implements Decoder.Text<ShowWithBookings> {

    private static Gson gson = new Gson();

    @Override
    public ShowWithBookings decode(String s) throws DecodeException {
        System.out.println("[BookingListDecoder] Received: " + s);
        ShowWithBookings SWBList = gson.fromJson(s, ShowWithBookings.class);
        // todo : decide if we want to use showExpanded or ShowWithBookings in the ShowPage !!!
        // for(ShowExpanded t_swb: SWBList.getShowsList())
        //     System.out.println(t_swb.toString());
        return SWBList;
    }

    @Override
    public boolean willDecode(String s) {
        return (s != null);
    }

    @Override
    public void init(EndpointConfig endpointConfig) {
        // Custom initialization logic
    }

    @Override
    public void destroy() {
        // Close resources
    }

}
