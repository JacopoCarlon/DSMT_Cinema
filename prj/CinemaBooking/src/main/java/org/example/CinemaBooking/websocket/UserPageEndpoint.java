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

@ServerEndpoint(value = "/user_page_endpoint/{username}", decoders = BookingListDecoder.class, encoders = BookingListEncoder.class)
public class UserPageEndpoint {

    private Session session  ;
    private static final Set<UserPageEndpoint> bookingEndpoints = new CopyOnWriteArraySet<UserPageEndpoint>();
    private static HashMap<String, String> users = new HashMap<String, String>();

    @OnOpen
    public void onOpen(Session session, @PathParam("username") String username) throws IOException, EncodeException {
        System.out.println("[MAIN MENU ENDPOINT] OnOpen");
        this.session = session;
        bookingEndpoints.add(this);
        users.put(session.getId(), username);
        printEndpointStatus();
    }

    @OnMessage
    public void onMessage(Session session, BookingList boookingList) throws IOException, EncodeException {
        System.out.println("[MAIN MENU ENDPOINT] OnMessage");
        System.out.println("[MAIN MENU ENDPOINT] Bookings list is going to be broadcast");
        broadcast(boookingList);
    }

    @OnClose
    public void onClose(Session session) throws IOException, EncodeException {
        System.out.println("[MAIN MENU ENDPOINT] OnClose: " + users.get(session.getId()) + " is exiting");
        bookingEndpoints.remove(this);
        users.remove(session.getId());
        printEndpointStatus();
    }

    @OnError
    public void onError(Session session, Throwable throwable) {
        // Do error handling here
    }

    private static void broadcast(BookingList bookingList) throws IOException, EncodeException {
        bookingEndpoints.forEach(endpoint -> {
            synchronized (endpoint) {
                try {
                    endpoint.session.getBasicRemote()
                            .sendObject(bookingList);
                } catch (IOException | EncodeException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    private static void printEndpointStatus(){
        System.out.println("[MAIN MENU ENDPOINT] User connected:");
        for(String user: users.values()){
            System.out.println(" user: " + user);
        }
    }

}

