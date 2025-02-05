package org.example.CinemaBooking.websocket;

import com.google.gson.Gson;
import jakarta.websocket.EncodeException;
import jakarta.websocket.Encoder;
import jakarta.websocket.EndpointConfig;
import org.example.CinemaBooking.dto.ShowList;

public class ShowListEncoder implements Encoder.Text<ShowList> {

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
