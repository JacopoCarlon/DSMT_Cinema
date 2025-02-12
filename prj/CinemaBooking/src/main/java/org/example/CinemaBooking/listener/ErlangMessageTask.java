package org.example.CinemaBooking.listener;

import com.ericsson.otp.erlang.*;
import com.google.gson.Gson;
import org.example.CinemaBooking.dto.Show;
import org.example.CinemaBooking.dto.ShowList;
import org.example.CinemaBooking.dto.ShowWithBookings;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.util.ArrayList;


public class ErlangMessageTask implements Runnable {
    private OtpErlangObject message;
    //TODO FOR LOCAL CONF:  change with "localhost:8080/CinemaBooking"
    //TODO FOR REMOTE CONF: change with "172.17.0.2:8080/CinemaBooking"
    private static final String base_uri = "172.17.0.2:8080/CinemaBooking";


    public ErlangMessageTask(OtpErlangObject inputErlangMsg) {
        this.message = inputErlangMsg;
    }

    @Override
    public void run() {
        if(message instanceof OtpErlangTuple){
            //{self(), destinationAtom, Result}
            OtpErlangAtom destination_atom = (OtpErlangAtom) ((OtpErlangTuple) message).elementAt(1);

            if (destination_atom.equals("available_shows_list")){
                System.out.println("[JAVA LISTENER] Refresh show list");
                // Result = {true, showList}
                OtpErlangTuple resultTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(2);
                OtpErlangList resultList = (OtpErlangList) resultTuple.elementAt(1);
                refreshBrowseShowsPage(resultList);
            }
            else if (destination_atom.equals("update_show_state")){
                System.out.println("[JAVA LISTENER] Refresh show state");
                // Result = [show data]
                OtpErlangList resultList = (OtpErlangList) ((OtpErlangTuple) message).elementAt(2);
                refreshShowState(resultList);
            }
        }
    }

    private void refreshBrowseShowsPage(OtpErlangList resultList) {
        WebsocketClientEndpoint clientEndpoint = null;
        try {
            clientEndpoint = new WebsocketClientEndpoint(new URI("ws://" + base_uri + "/browse_shows_endpoint/listener/listener"));
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }

        ArrayList<Show> tempShowList = new ArrayList<>();
        for (OtpErlangObject showObj : resultList) {
            Show show = Show.decodeFromErlangList((OtpErlangList) showObj);
            System.out.println("[JAVA LISTENER] Fetched: " + show);
            tempShowList.add(show);
        }

        ShowList showList = new ShowList(tempShowList);
        clientEndpoint.sendMessage(new Gson().toJson(showList));
        try {
            clientEndpoint.userSession.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void refreshShowState(OtpErlangList resultList) {
        WebsocketClientEndpoint clientEndpoint = null;
        ShowWithBookings showWB = ShowWithBookings.decodeFromErlangList(resultList);
        try {
            clientEndpoint = new WebsocketClientEndpoint(new URI("ws://" + base_uri + "/show_page_endpoint/" +
                    URLEncoder.encode(showWB.getShowID().toString(), "UTF-8") + "/listener/listener"));
        } catch (URISyntaxException | IOException e) {
            e.printStackTrace();
        }

        System.out.println("[JAVA LISTENER] Fetched: " + showWB);
        clientEndpoint.sendMessage(new Gson().toJson(showWB));
        try {
            clientEndpoint.userSession.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

