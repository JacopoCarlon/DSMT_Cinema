package org.example.CinemaBooking.websocket;

import org.example.CinemaBooking.dto.*;

import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.Collection;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArraySet;



@ServerEndpoint(value = "/show_page_endpoint/{show_id}/{user_type}/{user_identifier}",
        decoders = ShowWithBookingsDecoder.class,
        encoders = ShowWithBookingsEncoder.class)
public class ShowPageEndpoint {
    private Session session;
    private static final ConcurrentMap<String, Set<UserEndpointTuple>> clientEndpointsOfShow = new ConcurrentHashMap<>();

    @OnOpen
    public void onOpen(
            Session session,
            @PathParam("show_id") String showIdString,
            @PathParam("user_type") String userType,
            @PathParam("user_identifier") String userIdentifier
    ) throws IOException, EncodeException {
        System.out.println("[SHOW PAGE ENDPOINT] OnOpen of show: " + showIdString + ", from " + userType + ": " + userIdentifier);
        if (!clientEndpointsOfShow.containsKey(showIdString)) {
            Set<UserEndpointTuple> userEndpointTupleSet = new CopyOnWriteArraySet<>();
            userEndpointTupleSet.add(new UserEndpointTuple(userIdentifier, userType, this));
            clientEndpointsOfShow.put(showIdString, userEndpointTupleSet);
        }
        else {
            clientEndpointsOfShow.get(showIdString).add(new UserEndpointTuple(userIdentifier, userType, this));
        }
        printEndpointStatus();
    }


    @OnMessage
    public void onMessage(Session session, ShowWithBookings showWithBookings) throws IOException, EncodeException {
        System.out.println("[SHOW PAGE ENDPOINT] OnMessage");
        broadcast(showWithBookings);
    }


    @OnClose
    public void onClose(
            Session session,
            @PathParam("show_id") String showIdString,
            @PathParam("user_type") String userType,
            @PathParam("user_identifier") String userIdentifier
    ) throws IOException, EncodeException {
        System.out.println("[SHOW PAGE ENDPOINT] OnClose of show: " + showIdString + ", from " + userType + ": " + userIdentifier);
        Set<UserEndpointTuple> showEndpointsSet = clientEndpointsOfShow.get(showIdString);
        boolean result = showEndpointsSet.remove(new UserEndpointTuple(userIdentifier, userType, this));
        System.out.println("[SHOW PAGE ENDPOINT] OnClose result: " + result);
        printEndpointStatus();
    }


    @OnError
    public void onError(Session session, Throwable throwable) {
        // Do error handling here
        System.err.println("WebSocket error: " + throwable.getMessage());
    }


    private static void broadcast(ShowWithBookings showWithBookings) throws IOException, EncodeException {
        System.out.println("[SHOW PAGE ENDPOINT] Broadcasting state of show: " + showWithBookings.getShowID());
        Collection<UserEndpointTuple> userEndpointsSet = clientEndpointsOfShow.get(showWithBookings.getShowID().toString());
        userEndpointsSet.forEach(userEndpointTuple -> {
            ShowWithBookings showToSend = userEndpointTuple.getUserType().equals("customer") ?
                    showWithBookings.getPersonalizedCopy(userEndpointTuple.getUserID()) :
                    showWithBookings;

            System.out.println(userEndpointTuple.getUserType() + " " + userEndpointTuple.getUserID() + " will receive a message");
            try {
                userEndpointTuple.getEndpoint().session.getBasicRemote().sendObject(showToSend);
            } catch (IOException | EncodeException e) {
                e.printStackTrace();
            }
        });
    }


    private static void printEndpointStatus(){
        System.out.println("[SHOW PAGE ENDPOINT] Show Endpoint status");
        for (String showID : clientEndpointsOfShow.keySet()) {
            Collection<UserEndpointTuple> userEndpointsSet = clientEndpointsOfShow.get(showID);

            for (UserEndpointTuple userEndpointTuple : userEndpointsSet) {
                System.out.println("\t[Show " + showID + "] " + userEndpointTuple.getUserType() + ": " + userEndpointTuple.getUserID());
            }
        }
    }

}
