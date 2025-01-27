package org.example.CinemaBooking.communication;

import com.ericsson.otp.erlang.*;
import org.example.CinemaBooking.Constants;

import jakarta.servlet.http.HttpSession;
import org.example.CinemaBooking.dto.Booking;
import org.example.CinemaBooking.dto.Cinema;
import org.example.CinemaBooking.dto.Customer;
import org.example.CinemaBooking.dto.Show;

import java.awt.print.Book;
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
    public boolean registerNewCustomer(HttpSession session, String userName, String userPwd  ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform registerNewCustomer");
        send(session, serverRegisteredPID, new OtpErlangAtom("register_customer"), new OtpErlangString(userName), new OtpErlangString(userPwd) );
        return receiveRequestResult(session);
    }

    // loginExistingCustomer (username, password ) -> booleanResult, erroMsg
    public boolean loginExistingCustomer(HttpSession session, String userName, String userPwd ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform loginExistingCustomer");
        send(session, serverRegisteredPID, new OtpErlangAtom("login_customer"), new OtpErlangString(userName), new OtpErlangString(userPwd) );
        return receiveRequestResult(session);
    }

    // registerNewCinema ( cinemaName, cinemaPassword, address ) -> booleanResult, erroMsg
    public boolean registerNewCinema(HttpSession session, String cinemaName, String cinemaPwd, String cinemaAddress ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform registerNewCinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("register_cinema"), new OtpErlangString(cinemaName), new OtpErlangString(cinemaPwd), new OtpErlangString(cinemaAddress) );
        return receiveRequestResult(session);
    }

    // loginExistingCinema (cinemaID, password ) -> booleanResult, erroMsg
    public boolean loginExistingCinema(HttpSession session, String cinemaID, String cinemaPwd ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform loginExistingCinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("login_cinema"), new OtpErlangString(cinemaID), new OtpErlangString(cinemaPwd) );
        return receiveRequestResult(session);
    }


    // -----------------------------------------------------------------------------------------------
    // CINEMA PAGE + ACTIONS --------------------------------------------------------------------------------------- :

    // get_shows_by_cinema(cinemaID) -> showID, show_name, timestamp
    public List<Show> get_shows_by_cinema(HttpSession session, String cinemaID ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform get_shows_by_cinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("get_cinema_shows"), new OtpErlangString(cinemaID) );
        return receiveShowOfCinema(session);
    }

    // deleteShowFromCinema(cinemaID, showID) -> booleanResult, erroMsg
    public boolean deleteShowFromCinema(HttpSession session, String cinemaID , String showID ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform deleteShowFromCinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("deleteShowFromCinema"), new OtpErlangString(cinemaID) , new OtpErlangString(showID) );
        return receiveRequestResult(session);
    }

    // createNewShowForCinema(cinemaID, showName, showDate, maxSeats,) -> booleanResult, showID, erroMsg
    public boolean createNewShowForCinema(HttpSession session, String cinemaID, Show trg_show) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform createNewShowForCinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("add_show"), new OtpErlangString(cinemaID) , trg_show.toOtpErlangMap());
        return receiveRequestResult(session);
    }

    // find_cinema_by_name("String") -> lista di cinema con quel nome (location, nomeCinema)
    public List<Cinema> find_cinema_by_name(HttpSession session, String cinemaName ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform find_cinema_by_name");
        send(session, serverRegisteredPID, new OtpErlangAtom("findCinemaFromCinemaName"), new OtpErlangString(cinemaName) );
        return receiveListOfCinemas(session);
    }



    // -----------------------------------------------------------------------------------------------
    // USER PAGE --------------------------------------------------------------------------------------- :

    // // get_customer_data(userName) -> userName, userBookings (lista di : showID, nomeShow cinemaShow dataShow numPostPrenotati )
    // get_shows_by_customer(username) -> showID, show_name, timestamp, ...
    public List<Booking> get_shows_by_Customer(HttpSession session, Customer trg_customer) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform get_shows_by_customer");
        send(session, serverRegisteredPID, new OtpErlangAtom("getBookingsOfCustomer"), new OtpErlangString(trg_customer.getUsername()) );
        return receiveBookingsListOfCustomer(session);
    }


    // send_booking(showID, userName, nuovo_numero_booking_da_utente )
    public boolean send_booking_by_Customer(HttpSession session, Booking trg_booking) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform send_booking_by_Customer");
        send(session, serverRegisteredPID, new OtpErlangAtom("sendCreateUpdateBookingByCustomer"), trg_booking.toOtpErlangMap());
        return receiveRequestResult(session);
    }


    // delete booking == send_booking(showID, userName, 0 )
    public boolean delete_booking_of_Customer(HttpSession session, Booking trg_booking) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform delete_booking_by_Customer");
        trg_booking.setBookingSeats(0);
        return send_booking_by_Customer(session, trg_booking );
    }

    // logout




    // -----------------------------------------------------------------------------------------------
    // SHOW PAGE --------------------------------------------------------------------------------------- :

    // get_show_data(showID, userName) -> showID, show_name, timestamp, num_occupati, num_massimo, posti_bookati_da_utente
    public Booking get_customer_show_booking_data(HttpSession session, Show trg_show, Customer trg_customer) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform send_booking_by_Customer");
        send(session, serverRegisteredPID, new OtpErlangAtom("createUpdateBookingByCustomer"), trg_show.showIDtoOtpErlangMap(), trg_customer.customerNameToOtpErlangMap() );
        List<Booking> bookings_user_showID =  receiveBookingsListOfCustomer(session);
        if( bookings_user_showID.size() == 1 ){
            return bookings_user_showID.get(0);
        }
        if ( bookings_user_showID.isEmpty() ){
            return null;
        }
        return null;
    }






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

    // need somebody to tell me the showHandlerPID
    public void sendToPid(HttpSession session, OtpErlangPid showHandlerPID, OtpErlangObject... values){
        OtpMbox otpMbox = OtpMboxSingleton.getInstance(session);
        System.out.println("Created mbox with name: " + otpMbox.getName());
        OtpErlangTuple request = send_setup(otpMbox, session, values);
        otpMbox.send(showHandlerPID, request);
        System.out.println("Sent request " + request + " at server " + showHandlerPID.toString());
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

    public List<Cinema> receiveListOfCinemas(HttpSession session) throws OtpErlangDecodeException, OtpErlangExit {
        List<Cinema> cinemaList = new ArrayList<>();
        OtpErlangAtom status = new OtpErlangAtom("");
        OtpErlangObject message = receive_setup(session, receiveFetchMS);
        System.out.println("Receiving request result... ");
        if(message instanceof OtpErlangTuple){
            OtpErlangPid serverPID = (OtpErlangPid) ((OtpErlangTuple) message).elementAt(0);
            OtpErlangTuple resulTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(1);
            status = (OtpErlangAtom) (resulTuple).elementAt(0);
            OtpErlangList resultList = (OtpErlangList) (resulTuple).elementAt(1);

            for(OtpErlangObject result : resultList){
                Cinema trg_cinema = Cinema.decodeFromErlangList((OtpErlangList) result);
                System.out.println("Fetched: " + trg_cinema);
                cinemaList.add(trg_cinema);
            }
        }
        return cinemaList;
    }


    public List<Booking> receiveBookingsListOfCustomer(HttpSession session) throws OtpErlangDecodeException, OtpErlangExit {
        List<Booking> usersBookingsList = new ArrayList<>();
        OtpErlangAtom status = new OtpErlangAtom("");
        OtpErlangObject message = receive_setup(session, receiveFetchMS);
        System.out.println("Receiving request result... ");
        if(message instanceof OtpErlangTuple){
            OtpErlangPid serverPID = (OtpErlangPid) ((OtpErlangTuple) message).elementAt(0);
            OtpErlangTuple resulTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(1);
            status = (OtpErlangAtom) (resulTuple).elementAt(0);
            OtpErlangList resultList = (OtpErlangList) (resulTuple).elementAt(1);

            for(OtpErlangObject result : resultList){
                Booking trg_booking = Booking.decodeFromErlangList((OtpErlangList) result);
                System.out.println("Fetched: " + trg_booking);
                usersBookingsList.add(trg_booking);
            }
        }
        return usersBookingsList;
    }





}
