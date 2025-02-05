package org.example.CinemaBooking.websocket;

import com.google.gson.Gson;
import jakarta.websocket.DecodeException;
import jakarta.websocket.Decoder;
import jakarta.websocket.EndpointConfig;
import org.example.CinemaBooking.dto.Show;
import org.example.CinemaBooking.dto.ShowList;

public class ShowListDecoder implements Decoder.Text<ShowList> {

    private static Gson gson = new Gson();

    @Override
    public ShowList decode(String s) throws DecodeException {
        System.out.println("[ShowListDecoder] Received: " + s);
        ShowList showList = gson.fromJson(s, ShowList.class);
        for(Show t_show: showList.getShowsList())
            System.out.println(t_show.toString());
        return showList;
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
