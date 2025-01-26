package org.example.CinemaBooking.communication;

import com.ericsson.otp.erlang.*;
import org.example.CinemaBooking.Constants;

import jakarta.servlet.http.HttpSession;
import org.example.CinemaBooking.dto.Cinema;
import org.example.CinemaBooking.dto.Customer;
import org.example.CinemaBooking.dto.Show;

import java.util.ArrayList;
import java.util.List;

public class JE_CommunicationHandler {
    private static final String serverNode = "server@localhost";
    private static final String serverRegisteredPID = "main_server_endpoint";
    private static final int receiveTimeoutMS = 5000;
    private static final int receiveFetchMS = 100;


    // -----------------------------------------------------------------------------------------------
    // REGISTRATION --------------------------------------------------------------------------------------- :

    // registerNewCustomer ( username, password ) -> booleanResult, erroMsg
    public boolean registerNewCustomer(HttpSession session, Customer trg_customer) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform User SignUp");
        send(session, serverRegisteredPID, new OtpErlangAtom("customerRegister"), trg_customer.toOtpErlangMap());
        return receiveRequestResult(session);
    }

    // loginExistingCustomer (username, password ) -> booleanResult, erroMsg
    public boolean loginExistingCustomer(HttpSession session, Customer trg_customer) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform User SignUp");
        send(session, serverRegisteredPID, new OtpErlangAtom("customerLogin"), trg_customer.toOtpErlangMap());
        return receiveRequestResult(session);
    }

    // registerNewCinema ( cinemaName, cinemaPassword, address ) -> booleanResult, erroMsg
    public boolean registerNewCinema(HttpSession session, Cinema trg_cinema) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform User SignUp");
        send(session, serverRegisteredPID, new OtpErlangAtom("cinemaRegister"), trg_cinema.toOtpErlangMapNoShows());
        return receiveRequestResult(session);
    }

    // loginExistingCinema (username, password ) -> booleanResult, erroMsg
    public boolean loginExistingCinema(HttpSession session, Cinema trg_cinema) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform User SignUp");
        send(session, serverRegisteredPID, new OtpErlangAtom("cinemaLogin"), trg_cinema.toOtpErlangMapNoShows());
        return receiveRequestResult(session);
    }


    // -----------------------------------------------------------------------------------------------
    // CINEMA PAGE + ACTIONS --------------------------------------------------------------------------------------- :

    // get_cinema_shows(cinemaID) -> showID, show_name, timestamp
    public List<Show> get_cinema_shows(HttpSession session, Cinema trg_cinema) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform User SignUp");
        send(session, serverRegisteredPID, new OtpErlangAtom("getShowOfCinema"), trg_cinema.toOtpErlangMapNoShows());
        return receiveShowOfCinema(session);
    }

    // deleteShowFromCinema(cinemaID, showID) -> booleanResult, erroMsg
    public boolean deleteShowFromCinema(HttpSession session, Cinema trg_cinema, Show trg_show) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform User SignUp");
        send(session, serverRegisteredPID, new OtpErlangAtom("deleteShowFromCinema"), trg_cinema.toOtpErlangMapNoShows(), trg_show.toOtpErlangMap());
        return receiveRequestResult(session);
    }

    // createNewShowForCinema(cinemaID, showName, showDate, maxSeats,) -> booleanResult, showID, erroMsg
    public boolean createNewShowForCinema(HttpSession session, Cinema trg_cinema, Show trg_show) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform User SignUp");
        send(session, serverRegisteredPID, new OtpErlangAtom("addShowToCinema"), trg_cinema.toOtpErlangMapNoShows(), trg_show.toOtpErlangMap());
        return receiveRequestResult(session);
    }

    // find_cinema_by_name("String") -> lista di cinema con quel nome (location, nomeCinema)



    // -----------------------------------------------------------------------------------------------
    // USER PAGE --------------------------------------------------------------------------------------- :

    // get_customer_data(userName) -> userName, userBookings (lista di : showID, nomeShow cinemaShow dataShow numPostPrenotati )

    // delete booking == send_booking(showID, userName, 0 )

    // logout




    // -----------------------------------------------------------------------------------------------
    // SHOW PAGE --------------------------------------------------------------------------------------- :

    // get_show_data(showID, userName) -> showID, show_name, timestamp, num_occupati, num_massimo, posti_bookati_da_utente

    // send_booking(showID, userName, nuovo_numero_booking_da_utente )
















    // --- main utility functions ---

    // -----------------------------------------------------------------------------------------------
    // SEND --------------------------------------------------------------------------------------- :

    private OtpErlangTuple send_setup(OtpMbox otpMbox, HttpSession session, OtpErlangObject... values){
        OtpErlangObject[] arr = new OtpErlangObject[values.length + 1];
        arr[0] = otpMbox.self();
        System.arraycopy(values, 0, arr, 1, values.length);
        return new OtpErlangTuple(arr);
    }

    public void send(HttpSession session, String serverRegisteredPID, OtpErlangObject... values){
        OtpMbox otpMbox = OtpMboxSingleton.getInstance(session);
        System.out.println("Created mbox with name: " + otpMbox.getName());
        OtpErlangTuple request = send_setup(otpMbox, session, values);
        otpMbox.send(serverRegisteredPID, serverNode, request);
        System.out.println("Sent request " + request + " at server " + serverRegisteredPID);
    }

    public void sendToPid(HttpSession session, OtpErlangPid auctionHandlerPID, OtpErlangObject... values){
        OtpMbox otpMbox = OtpMboxSingleton.getInstance(session);
        System.out.println("Created mbox with name: " + otpMbox.getName());
        OtpErlangTuple request = send_setup(otpMbox, session, values);
        otpMbox.send(auctionHandlerPID, request);
        System.out.println("Sent request " + request + " at server " + auctionHandlerPID.toString());
    }


    // -----------------------------------------------------------------------------------------------
    // RECEIVE --------------------------------------------------------------------------------------- :

    private OtpErlangObject receive_setup(HttpSession session, int timeout) throws OtpErlangDecodeException, OtpErlangExit {
        OtpMbox otpMbox = OtpMboxSingleton.getInstance(session);
        return otpMbox.receive(timeout);
    }

    public boolean receiveRequestResult(HttpSession session) throws OtpErlangDecodeException, OtpErlangExit {
        OtpErlangAtom status = new OtpErlangAtom("");
        OtpErlangObject message = receive_setup(session, receiveTimeoutMS);
        System.out.println("Receiving request result... ");
        if(message instanceof OtpErlangTuple){
            OtpErlangPid serverPID = (OtpErlangPid) ((OtpErlangTuple) message).elementAt(0);
            OtpErlangTuple resulTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(1);
            status = (OtpErlangAtom) (resulTuple).elementAt(0);
        }
        return status.toString().equals("ok");
    }

    public List<Show> receiveShowOfCinema(HttpSession session) throws OtpErlangDecodeException, OtpErlangExit {
        List<Show> showList = new ArrayList<>();
        OtpErlangAtom status = new OtpErlangAtom("");
        OtpErlangObject message = receive_setup(session, receiveFetchMS);
        System.out.println("Receiving request result... ");
        if(message instanceof OtpErlangTuple){
            OtpErlangPid serverPID = (OtpErlangPid) ((OtpErlangTuple) message).elementAt(0);
            OtpErlangTuple resulTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(1);
            status = (OtpErlangAtom) (resulTuple).elementAt(0);
            OtpErlangList resultList = (OtpErlangList) (resulTuple).elementAt(1);

            for(OtpErlangObject result : resultList){
                Show trg_show = Show.decodeFromErlangList((OtpErlangList) result);
                System.out.println("Fetched: " + trg_show);
                showList.add(trg_show);
            }
        }
        return showList;
    }







}
