package org.example.CinemaBooking.websocket;

import org.example.CinemaBooking.dto.Booking;
import org.example.CinemaBooking.dto.BookingList;

import com.google.gson.Gson;
import com.google.gson.JsonElement;

import jakarta.websocket.EndpointConfig;
import jakarta.websocket.EncodeException;
import jakarta.websocket.Encoder;


public class BookingListEncoder implements Encoder.Text<BookingList> {
    private static Gson gson = new Gson();

    @Override
    public String encode(BookingList message) throws EncodeException {
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
