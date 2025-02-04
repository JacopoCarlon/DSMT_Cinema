package org.example.CinemaBooking.websocket;

import org.example.CinemaBooking.dto.Show;
import org.example.CinemaBooking.dto.ShowExpandedList;

import com.google.gson.Gson;

import jakarta.websocket.EndpointConfig;
import jakarta.websocket.DecodeException;
import jakarta.websocket.Decoder;
public class ShowExpandedListDecoder implements Decoder.Text<ShowExpandedList> {

    private static Gson gson = new Gson();

    @Override
    public ShowExpandedList decode(String s) throws DecodeException {
        System.out.println("[ShowListDecoder] Received: " + s);
        ShowExpandedList showExpandedList = gson.fromJson(s, ShowExpandedList.class);
        for(Show t_show: showExpandedList.getShowsList())
            System.out.println(t_show.toString());
        return showExpandedList;
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
