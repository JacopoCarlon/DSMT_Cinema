package org.example.CinemaBooking.websocket;

import org.example.CinemaBooking.dto.Booking;
import org.example.CinemaBooking.dto.BookingList;

import com.google.gson.Gson;
import com.google.gson.JsonElement;

import jakarta.websocket.EndpointConfig;
import jakarta.websocket.DecodeException;
import jakarta.websocket.Decoder;


public class BookingListDecoder implements Decoder.Text<BookingList>{
    private static Gson gson = new Gson();

    @Override
    public BookingList decode(String s) throws DecodeException {
        System.out.println("[BookingListDecoder] Received: " + s);
        BookingList bookingList = gson.fromJson(s, BookingList.class);
        for(Booking t_booking: bookingList.getBookingsList())
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
