package org.example.CinemaBooking.websocket;

import org.example.CinemaBooking.dto.*;

import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;


// todo

@ServerEndpoint(value = "/show_page_endpoint/{username}", decoders = ShowWithBookingsDecoder.class, encoders = ShowWithBookingsEncoder.class)
public class ShowPageEndpoint {

    // todo :
    //  need userendpointpair + cinemaendpoint pair ???
    // need more study theory

    private Session session ;
    private static final Set<ShowPageEndpoint> showPagesEndpoints = new CopyOnWriteArraySet<ShowPageEndpoint>();
    private static HashMap<String, String> users = new HashMap<String, String>();

    @OnOpen
    public void onOpen(Session session, @PathParam("username") String username) throws IOException, EncodeException {
        System.out.println("[UserPageEndpoint] OnOpen");
        this.session = session;
        showPagesEndpoints.add(this);
        users.put(session.getId(), username);
        printEndpointStatus();
    }

    /*
    TODO: check this
    */
    @OnMessage
    public void onMessage(Session session, ShowWithBookings showWithBookings) throws IOException, EncodeException {
        System.out.println("[UserPageEndpoint] OnMessage");
        System.out.println("[UserPageEndpoint] Bookings list is going to be broadcast");
        broadcast(showWithBookings);
    }


    @OnClose
    public void onClose(Session session) throws IOException, EncodeException {
        System.out.println("[UserPageEndpoint] OnClose: " + users.get(session.getId()) + " is exiting");
        showPagesEndpoints.remove(this);
        users.remove(session.getId());
        printEndpointStatus();
    }

    @OnError
    public void onError(Session session, Throwable throwable) {
        // Do error handling here
    }

    /*
    TODO: check this
    */
    private static void broadcast(ShowWithBookings showWithBookings) throws IOException, EncodeException {
        showPagesEndpoints.forEach(endpoint -> {
            synchronized (endpoint) {
                try {
                    endpoint.session.getBasicRemote()
                            .sendObject(showWithBookings);
                } catch (IOException | EncodeException e) {
                    e.printStackTrace();
                }
            }
        });
    }


    private static void printEndpointStatus(){
        System.out.println("[UserPageEndpoint] User connected:");
        for(String user: users.values()){
            System.out.println(" user : " + user);
        }
    }

}
