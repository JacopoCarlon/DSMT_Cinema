package org.example.CinemaBooking.websocket;

import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;
import org.example.CinemaBooking.dto.ShowList;

import java.io.IOException;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

@ServerEndpoint(value = "/browse_shows_endpoint/{user_type}/{user_identifier}", decoders = ShowListDecoder.class, encoders = ShowListEncoder.class)
public class BrowseShowsPageEndpoint {

    private Session session ;
    private static final Set<BrowseShowsPageEndpoint> browseShowsEndpoints = new CopyOnWriteArraySet<BrowseShowsPageEndpoint>();
    private static HashMap<String, String> customers = new HashMap<String, String>();
    private static HashMap<String, String> cinemas = new HashMap<String, String>();

    @OnOpen
    public void onOpen(Session session,
                       @PathParam("user_type") String userType,
                       @PathParam("user_identifier") String userIdentifier
    ) throws IOException, EncodeException {
        System.out.println("[BROWSE PAGE ENDPOINT] OnOpen from " + userType + ": " + userIdentifier);
        this.session = session;
        browseShowsEndpoints.add(this);

        if ("customer".equals(userType))
            customers.put(session.getId(), userIdentifier);
        else if ("cinema".equals(userType))
            cinemas.put(session.getId(), userIdentifier);

        printEndpointStatus();
    }

    @OnMessage
    public void onMessage(Session session, ShowList showList) throws IOException, EncodeException {
        System.out.println("[BROWSE PAGE ENDPOINT] OnMessage");
        broadcast(showList);
    }

    @OnClose
    public void onClose(Session session) throws IOException, EncodeException {
        if (customers.containsKey(session.getId())) {
            System.out.println("[BROWSE PAGE ENDPOINT] OnClose: customer '" + customers.get(session.getId()) + "' is exiting");
            customers.remove(session.getId());
        }
        else if (cinemas.containsKey(session.getId())) {
            System.out.println("[BROWSE PAGE ENDPOINT] OnClose: cinema " + cinemas.get(session.getId()) + " is exiting");
            cinemas.remove(session.getId());
        }
        browseShowsEndpoints.remove(this);
        printEndpointStatus();
    }

    @OnError
    public void onError(Session session, Throwable throwable) {
        // Do error handling here
    }

    private static void broadcast(ShowList showList) throws IOException, EncodeException {
        browseShowsEndpoints.forEach(endpoint -> {
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
        System.out.println("[BROWSE PAGE ENDPOINT] Users connected:");
        for(String cust: customers.values()){
            System.out.println(" - customer : " + cust);
        }
        for(String cin: cinemas.values()){
            System.out.println(" - cinema : " + cin);
        }
    }

}
