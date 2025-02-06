package org.example.CinemaBooking.websocket;

import com.google.gson.Gson;

import jakarta.websocket.*;
import org.example.CinemaBooking.dto.ShowWithBookings;

public class ShowWithBookingsEncoder implements Encoder.Text<ShowWithBookings> {
    private static Gson gson = new Gson();

    @Override
    public String encode(ShowWithBookings message) throws EncodeException {
        String json = gson.toJson(message);
        System.out.println("Message encoded in JSON: " + message);
        return json;
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
