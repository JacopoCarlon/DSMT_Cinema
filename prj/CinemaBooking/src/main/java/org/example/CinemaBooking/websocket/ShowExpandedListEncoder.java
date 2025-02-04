package org.example.CinemaBooking.websocket;

import org.example.CinemaBooking.dto.ShowExpandedList;

import com.google.gson.Gson;

import jakarta.websocket.EndpointConfig;
import jakarta.websocket.EncodeException;
import jakarta.websocket.Encoder;

public class ShowExpandedListEncoder implements Encoder.Text<ShowExpandedList>{

    private static Gson gson = new Gson();

    @Override
    public String encode(ShowExpandedList message) throws EncodeException {
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
