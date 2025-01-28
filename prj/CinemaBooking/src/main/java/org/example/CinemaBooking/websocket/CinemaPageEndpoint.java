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

@ServerEndpoint(value = "/cinema_page_endpoint/{username}", decoders = ShowListDecoder.class, encoders = ShowListEncoder.class)
public class CinemaPageEndpoint {

    private Session session ;
    private static final Set<CinemaPageEndpoint> showEndpoints = new CopyOnWriteArraySet<CinemaPageEndpoint>();
    private static HashMap<String, String> cinemas = new HashMap<String, String>();

    @OnOpen
    public void onOpen(Session session, @PathParam("username") String username) throws IOException, EncodeException {
        System.out.println("[CinemaPageEndpoint] OnOpen");
        this.session = session;
        showEndpoints.add(this);
        cinemas.put(session.getId(), username);
        printEndpointStatus();
    }

    @OnMessage
    public void onMessage(Session session, ShowList showList) throws IOException, EncodeException {
        System.out.println("[CinemaPageEndpoint] OnMessage");
        System.out.println("[CinemaPageEndpoint] Show list is going to be broadcast");
        broadcast(showList);
    }

    @OnClose
    public void onClose(Session session) throws IOException, EncodeException {
        System.out.println("[CinemaPageEndpoint] OnClose: " + cinemas.get(session.getId()) + " is exiting");
        showEndpoints.remove(this);
        cinemas.remove(session.getId());
        printEndpointStatus();
    }

    @OnError
    public void onError(Session session, Throwable throwable) {
        // Do error handling here
    }

    private static void broadcast(ShowList showList) throws IOException, EncodeException {
        showEndpoints.forEach(endpoint -> {
            synchronized (endpoint) {
                try {
                    endpoint.session.getBasicRemote()
                            .sendObject(showList);
                } catch (IOException | EncodeException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    private static void printEndpointStatus(){
        System.out.println("[CinemaPageEndpoint] Cinema connected:");
        for(String cinema: cinemas.values()){
            System.out.println(" cinema : " + cinema);
        }
    }

}
