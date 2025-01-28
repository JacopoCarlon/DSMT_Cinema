package org.example.CinemaBooking.websocket;

import org.example.CinemaBooking.dto.*;

import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;


// todo

@ServerEndpoint(value = "/show_page_endpoint/{username}", decoders = BookingListDecoder.class, encoders = BookingListEncoder.class)
public class ShowPageEndpoint {

    // todo :
    //  need userendpointpair + cinemaendpoint pair ???
    // need more study theory

}
