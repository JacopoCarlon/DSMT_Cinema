package org.example.CinemaBooking.communication;

import com.ericsson.otp.erlang.*;

import jakarta.servlet.http.HttpSession;
import org.example.CinemaBooking.dto.*;

import java.util.ArrayList;
import java.util.List;

public class JE_CommunicationHandler {
    private static final String serverNode = "server@localhost";
    private static final String serverRegisteredPID = "main_server_endpoint";
    private static final int receiveTimeoutMS = 5000;
    private static final int receiveFetchMS = 100;


    // -----------------------------------------------------------------------------------------------
    // REGISTRATION --------------------------------------------------------------------------------------- :

    // registerNewCustomer ( username, password ) -> booleanResult
    public boolean registerNewCustomer(HttpSession session, String userName, String userPwd  ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform registerNewCustomer");
        send(session, serverRegisteredPID, new OtpErlangAtom("register_customer"), new OtpErlangString(userName), new OtpErlangString(userPwd) );
        return receiveRequestResult(session);
    }

    // loginExistingCustomer (username, password ) -> booleanResult
    public boolean loginExistingCustomer(HttpSession session, String userName, String userPwd ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform loginExistingCustomer");
        send(session, serverRegisteredPID, new OtpErlangAtom("login_customer"), new OtpErlangString(userName), new OtpErlangString(userPwd) );
        return receiveRequestResult(session);
    }

    // registerNewCinema ( cinemaName, cinemaPassword, address ) -> {false} / {true, newCinemaID}
    // TODO: change return value
    public Long registerNewCinema(HttpSession session, String cinemaName, String cinemaPwd, String cinemaAddress ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform registerNewCinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("register_cinema"), new OtpErlangString(cinemaName), new OtpErlangString(cinemaPwd), new OtpErlangString(cinemaAddress) );
        return receiveNumericID(session);
    }

    // loginExistingCinema (cinemaID, password ) -> booleanResult
    public boolean loginExistingCinema(HttpSession session, String cinemaID, String cinemaPwd ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform loginExistingCinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("login_cinema"), new OtpErlangString(cinemaID), new OtpErlangString(cinemaPwd) );
        return receiveRequestResult(session);
    }


    // -----------------------------------------------------------------------------------------------
    // CINEMA PAGE + ACTIONS --------------------------------------------------------------------------------------- :

    // get_shows_by_cinema(cinemaID) -> List<Show> --> {id, name, date, cinemaId, cinemaName, cinemaLocation, maxSeats, availSeats, isEnded}
    public List<Show> get_shows_by_cinema(HttpSession session, String cinemaID ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform get_shows_by_cinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("get_cinema_shows"), new OtpErlangString(cinemaID) );
        return receiveShowOfCinema(session);
    }

    /*
    // TODO: valutare se tenere o no
    // deleteShowFromCinema(cinemaID, showID) -> booleanResult, erroMsg
    public boolean deleteShowFromCinema(HttpSession session, String cinemaID , String showID ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform deleteShowFromCinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("deleteShowFromCinema"), new OtpErlangString(cinemaID) , new OtpErlangString(showID) );
        return receiveRequestResult(session);
    }
     */

    // createNewShowForCinema(cinemaID, showName, showDate, maxSeats,) -> {false} / {true, newShowID}
    public Long createNewShowForCinema(HttpSession session, String cinemaID, Show trg_show) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform createNewShowForCinema");
        send(session, serverRegisteredPID, new OtpErlangAtom("add_show"), new OtpErlangString(cinemaID) , trg_show.toOtpErlangMap());
        return receiveNumericID(session);
    }

    // find_cinema_by_name("String") -> List<Cinema> --> {id, name, location}
    public List<Cinema> find_cinema_by_name(HttpSession session, String cinemaName ) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform find_cinema_by_name");
        send(session, serverRegisteredPID, new OtpErlangAtom("find_cinema_by_name"), new OtpErlangString(cinemaName) );
        return receiveListOfCinemas(session);
    }



    // -----------------------------------------------------------------------------------------------
    // USER PAGE --------------------------------------------------------------------------------------- :

    // get_shows_by_customer(username) -> List<ShowExpanded>
    // --> {id, name, date, cinemaId, cinemaName, cinemaLocation, maxSeats, availSeats, isEnded(==false), committedBooking, waitingBooking(==null)}
    public List<ShowExpanded> get_shows_by_Customer(HttpSession session, String userName) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform get_shows_by_customer");
        send(session, serverRegisteredPID, new OtpErlangAtom("get_customer_bookings"), new OtpErlangString(userName) );
        return receiveBookingsListOfCustomer(session);
    }


    // send_booking({userName, nuovo_numero_booking_da_utente}) -> updated ShowExpanded
    public ShowExpanded send_booking_by_Customer(HttpSession session, OtpErlangPid showPid, CustomerBooking trg_booking) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform send_booking_by_Customer");
        // TODO: change dest pid
        sendToPid(session, showPid, new OtpErlangAtom("update_booking"), trg_booking.toOtpErlangMap());
        // TODO: function for ShowExpanded
        return receiveShowExpandedFromShowNode(session);
    }


    // delete booking == send_booking(showID, userName, 0 )
    public ShowExpanded delete_booking_of_Customer(HttpSession session, OtpErlangPid showPid,  CustomerBooking trg_booking) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform delete_booking_by_Customer");
        trg_booking.setBookingSeats(0L);
        return send_booking_by_Customer(session, showPid,  trg_booking);
    }

    // logout




    // -----------------------------------------------------------------------------------------------
    // SHOW PAGE --------------------------------------------------------------------------------------- :
/*
    // get_show_data(showID, userName) -> showID, show_name, timestamp, num_occupati, num_massimo, posti_bookati_da_utente
    public Booking get_customer_show_booking_data(HttpSession session, Show trg_show, Customer trg_customer) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to perform get_booking_by_Customer");
        // TODO: change Pid
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
*/

    // getShowPidFromBooking
    /* TODO: better remove this
    public OtpErlangPid getShowPidFromBooking(HttpSession session, ShowExpanded trg_booking) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to get show pid");
        send(session, serverRegisteredPID, new OtpErlangAtom("getShowPidFromBooking"), new OtpErlangLong(trg_booking.getShowID()));
        return receiveShowPid(session);
    }
     */

    public OtpErlangPid getShowPidFromId(HttpSession session, Long showId) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to get show pid");
        send(session, serverRegisteredPID, new OtpErlangAtom("getShowPidFromBooking"), new OtpErlangLong(showId));
        return receiveShowPid(session);
    }


    // getShowExpandedUpdated(username)
    // -> {id, name, date, cinemaId, cinemaName, cinemaLocation, maxSeats, availSeats, isEnded(==false), committedBooking, waitingBooking}
    // Used by customers to get a show page
    public ShowExpanded getShowExpandedUpdated(HttpSession session, OtpErlangPid showPid, String callerCustomer) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to get showExtended values");
        sendToPid(session, showPid, new OtpErlangAtom("get_data_for_customer"), new OtpErlangString(callerCustomer));
        return receiveShowExpandedFromShowNode(session);
    }

    // getShowWithBookingsUpdated(username)
    // -> {id, name, date, cinemaId, cinemaName, cinemaLocation, maxSeats, availSeats, isEnded, committedBookingsList, waitingBookingList}
    // Used by cinemas to get a show page. It gets all bookings along with available seats
    public ShowWithBookings getShowWithBookingsUpdated(HttpSession session, OtpErlangPid showPid, Long cinemaId) throws OtpErlangDecodeException, OtpErlangExit {
        System.out.println("Trying to get showExtended values");
        sendToPid(session, showPid, new OtpErlangAtom("get_data_for_cinema"), new OtpErlangLong(cinemaId));
        return receiveShowWithBookingsFromShowNode(session);
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
        return status.toString().equals("true");
    }


    // todo
    public OtpErlangPid receiveShowPid(HttpSession session) throws OtpErlangDecodeException, OtpErlangExit {
        OtpErlangAtom status;
        OtpMbox otpMbox = OtpMboxSingleton.getInstance(session);
        OtpErlangObject message = otpMbox.receive(receiveTimeoutMS);
        if(message instanceof OtpErlangTuple){
            // OtpErlangPid serverPID = (OtpErlangPid) ((OtpErlangTuple) message).elementAt(0);
            OtpErlangTuple resulTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(1);
            status = (OtpErlangAtom) (resulTuple).elementAt(0);
            if (status.toString().equals("false"))
                return null;
            OtpErlangList list = (OtpErlangList) (resulTuple).elementAt(1);
            return (OtpErlangPid) (list).elementAt(0);
        }
        return null;
    }

    // todo
    public Long receiveNumericID(HttpSession session) throws OtpErlangDecodeException, OtpErlangExit {
        OtpErlangAtom status;
        OtpMbox otpMbox = OtpMboxSingleton.getInstance(session);
        OtpErlangObject message = otpMbox.receive(receiveTimeoutMS);
        if(message instanceof OtpErlangTuple){
            // OtpErlangPid serverPID = (OtpErlangPid) ((OtpErlangTuple) message).elementAt(0);
            OtpErlangTuple resulTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(1);
            status = (OtpErlangAtom) (resulTuple).elementAt(0);
            if (status.toString().equals("false"))
                return null;
            OtpErlangList list = (OtpErlangList) (resulTuple).elementAt(1);
            return ((OtpErlangLong) (list).elementAt(0)).longValue();
        }
        return null;
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


    public ShowWithBookings receiveShowWithBookingsFromShowNode(HttpSession session) throws OtpErlangDecodeException, OtpErlangExit {
        OtpErlangAtom status = new OtpErlangAtom("");
        OtpErlangObject message = receive_setup(session, receiveTimeoutMS);
        System.out.println("Receiving request result... ");
        if(message instanceof OtpErlangTuple){
            OtpErlangPid serverPID = (OtpErlangPid) ((OtpErlangTuple) message).elementAt(0);
            OtpErlangTuple resulTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(1);
            status = (OtpErlangAtom) (resulTuple).elementAt(0);
            OtpErlangList resultShow = (OtpErlangList) (resulTuple).elementAt(1);

            return ShowWithBookings.decodeFromErlangList(resultShow);
        }
        return null;
    }


    public ShowExpanded receiveShowExpandedFromShowNode(HttpSession session) throws OtpErlangDecodeException, OtpErlangExit {
        OtpErlangAtom status = new OtpErlangAtom("");
        OtpErlangObject message = receive_setup(session, receiveTimeoutMS);
        System.out.println("Receiving request result... ");
        if(message instanceof OtpErlangTuple){
            OtpErlangPid serverPID = (OtpErlangPid) ((OtpErlangTuple) message).elementAt(0);
            OtpErlangTuple resulTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(1);
            status = (OtpErlangAtom) (resulTuple).elementAt(0);
            OtpErlangList resultShow = (OtpErlangList) (resulTuple).elementAt(1);

            return ShowExpanded.decodeFromErlangList(resultShow);
        }
        return null;
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


    public List<ShowExpanded> receiveBookingsListOfCustomer(HttpSession session) throws OtpErlangDecodeException, OtpErlangExit {
        List<ShowExpanded> usersBookingsList = new ArrayList<>();
        OtpErlangAtom status = new OtpErlangAtom("");
        OtpErlangObject message = receive_setup(session, receiveFetchMS);
        System.out.println("Receiving request result... ");
        if(message instanceof OtpErlangTuple){
            OtpErlangPid serverPID = (OtpErlangPid) ((OtpErlangTuple) message).elementAt(0);
            OtpErlangTuple resulTuple = (OtpErlangTuple) ((OtpErlangTuple) message).elementAt(1);
            status = (OtpErlangAtom) (resulTuple).elementAt(0);
            OtpErlangList resultList = (OtpErlangList) (resulTuple).elementAt(1);

            for(OtpErlangObject result : resultList){
                ShowExpanded trg_booking = ShowExpanded.decodeFromErlangList((OtpErlangList) result);
                System.out.println("Fetched: " + trg_booking);
                usersBookingsList.add(trg_booking);
            }
        }
        return usersBookingsList;
    }

}
