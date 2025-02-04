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

@ServerEndpoint(value = "/user_page_endpoint/{username}", decoders = ShowExpandedListDecoder.class, encoders = ShowExpandedListEncoder.class)
public class UserPageEndpoint {

    private Session session ;
    private static final Set<UserPageEndpoint> userPageEndpoints = new CopyOnWriteArraySet<UserPageEndpoint>();
    private static HashMap<String, String> users = new HashMap<String, String>();

    @OnOpen
    public void onOpen(Session session, @PathParam("username") String username) throws IOException, EncodeException {
        System.out.println("[UserPageEndpoint] OnOpen");
        this.session = session;
        userPageEndpoints.add(this);
        users.put(session.getId(), username);
        printEndpointStatus();
    }

    /*
    TODO: check this
    */
    @OnMessage
    public void onMessage(Session session, ShowExpandedList showExpandedList) throws IOException, EncodeException {
        System.out.println("[UserPageEndpoint] OnMessage");
        System.out.println("[UserPageEndpoint] ShowExpandedList list is going to be broadcast");
        broadcast(showExpandedList);
    }


    @OnClose
    public void onClose(Session session) throws IOException, EncodeException {
        System.out.println("[UserPageEndpoint] OnClose: " + users.get(session.getId()) + " is exiting");
        userPageEndpoints.remove(this);
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
    private static void broadcast(ShowExpandedList showExpandedList) throws IOException, EncodeException {
        userPageEndpoints.forEach(endpoint -> {
            synchronized (endpoint) {
                try {
                    endpoint.session.getBasicRemote()
                            .sendObject(showExpandedList);
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

