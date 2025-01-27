package org.example.CinemaBooking.websocket;

import org.example.CinemaBooking.dto.Show;
import org.example.CinemaBooking.dto.ShowList;

import com.google.gson.Gson;
import com.google.gson.JsonElement;

import jakarta.websocket.EndpointConfig;
import jakarta.websocket.EncodeException;
import jakarta.websocket.Encoder;

public class ShowListEncoder implements Encoder.Text<ShowList>{

    private static Gson gson = new Gson();

    @Override
    public String encode(ShowList message) throws EncodeException {
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
