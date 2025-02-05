package org.example.CinemaBooking.websocket;

import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;
import org.example.CinemaBooking.dto.ShowList;

import java.io.IOException;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

@ServerEndpoint(value = "/browse_shows_endpoint", decoders = ShowListDecoder.class, encoders = ShowListEncoder.class)
public class BrowseShowsPageEndpoint {

    private Session session ;
    private static final Set<BrowseShowsPageEndpoint> browseShowsEndpoints = new CopyOnWriteArraySet<BrowseShowsPageEndpoint>();
    private static HashMap<String, String> customers = new HashMap<String, String>();

    @OnOpen
    public void onOpen(Session session, @PathParam("username") String username) throws IOException, EncodeException {
        System.out.println("[BrowseShowsPageEndpoint] OnOpen");
        this.session = session;
        browseShowsEndpoints.add(this);
        customers.put(session.getId(), username);
        printEndpointStatus();
    }

    @OnMessage
    public void onMessage(Session session, ShowList showList) throws IOException, EncodeException {
        System.out.println("[BrowseShowsPageEndpoint] OnMessage");
        System.out.println("[BrowseShowsPageEndpoint] Show list is going to be broadcast");
        broadcast(showList);
    }

    @OnClose
    public void onClose(Session session) throws IOException, EncodeException {
        System.out.println("[BrowseShowsPageEndpoint] OnClose: " + customers.get(session.getId()) + " is exiting");
        browseShowsEndpoints.remove(this);
        customers.remove(session.getId());
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
        System.out.println("[BrowseShowsPageEndpoint] Customers connected:");
        for(String cust: customers.values()){
            System.out.println(" customer : " + cust);
        }
    }

}
