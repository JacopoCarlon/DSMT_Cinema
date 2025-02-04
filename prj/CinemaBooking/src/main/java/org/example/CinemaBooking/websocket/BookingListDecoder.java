package org.example.CinemaBooking.websocket;

import com.google.gson.Gson;

import jakarta.websocket.EndpointConfig;
import jakarta.websocket.DecodeException;
import jakarta.websocket.Decoder;
import org.example.CinemaBooking.dto.ShowExpanded;
import org.example.CinemaBooking.dto.ShowExpandedList;


public class BookingListDecoder implements Decoder.Text<ShowExpandedList>{
    private static Gson gson = new Gson();

    @Override
    public ShowExpandedList decode(String s) throws DecodeException {
        System.out.println("[BookingListDecoder] Received: " + s);
        ShowExpandedList bookingList = gson.fromJson(s, ShowExpandedList.class);
        for(ShowExpanded t_booking: bookingList.getShowsList())
            System.out.println(t_booking.toString());
        return bookingList;
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
