package org.example.CinemaBooking.servlets;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import org.example.CinemaBooking.communication.JE_CommunicationHandler;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.example.CinemaBooking.dto.*;


import java.io.IOException;
import java.util.List;
import java.util.Objects;

// this is similar to "CreateShowServlet", since we arrive after a good post.

@WebServlet(name = "ShowPageServlet", value = "/ShowPageServlet")
public class ShowPageServlet extends HttpServlet{

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        System.out.println("DoGet ShowPageServlet");
        // arrive here from POST by UserPageServlet or CinemaPageServlet

        String username = "";
        Long cinemaID = -1L;
        String is_a_cinema = request.getParameter("is_a_cinema");

        // does the query arrive from a cinema or a user ?
        boolean session_this_is_a_cinema = Objects.equals(is_a_cinema, "true");

        if(session_this_is_a_cinema){
            cinemaID = (Long) request.getSession().getAttribute("username");
        }else{
            username = (String) request.getSession().getAttribute("username");
        }

        Long trgShowID = (Long) request.getSession().getAttribute("getShowID");

        JE_CommunicationHandler communicationHandler = new JE_CommunicationHandler();
        OtpErlangPid pid = null;
        try {
            pid = communicationHandler.getShowPidFromId(request.getSession(), trgShowID);
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }

        if(pid != null){
            System.out.println("GET Show pid got: " + pid.toString());
            // // request.getSession().removeAttribute("getShowID");
            request.getSession().removeAttribute("currentShowPid");
            request.getSession().setAttribute("currentShowPid", pid);
            boolean good_to_move = false;

            ShowWithBookings gottenSWB = null;
            try {
                // we get the updated show data
                if (session_this_is_a_cinema){
                    gottenSWB = new JE_CommunicationHandler().getShowWithBookingsForCinema(request.getSession(), pid, cinemaID);
                }else{
                    gottenSWB = new JE_CommunicationHandler().getShowWithBookingsForCustomer(request.getSession(), pid, username);
                }

                if (gottenSWB != null){
                    good_to_move = true;
                }
            } catch (OtpErlangDecodeException | OtpErlangExit e) {
                e.printStackTrace();
            }

            if(good_to_move ){
                request.getSession().removeAttribute("currentSWB");
                request.getSession().setAttribute("currentSWB", gottenSWB);
                System.out.println("GET can properly load show page");
                response.sendRedirect(request.getContextPath() + "/ShowServlet");
            }else {
                System.out.println("GET something went wrong in ShowPageServlet.java ");
                request.getSession().removeAttribute("currentShowPid");
                request.getSession().removeAttribute("currentSWB");
            }
        }

        request.getSession().removeAttribute("bookingUpdateStatus");

        RequestDispatcher requestDispatcher = request.getRequestDispatcher("/jpages/show_page.jsp");
        requestDispatcher.forward(request, response);
    }


    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String is_this_a_cinema = (String) request.getSession().getAttribute("is_a_cinema");
        String sender_Name = (String) request.getSession().getAttribute("username");

        ShowExpanded old_show_expanded = (ShowExpanded) request.getSession().getAttribute("currentShowExpanded");
        long old_showID = old_show_expanded.getShowID();
        String old_showName = old_show_expanded.getShowName();
        String old_showDate = old_show_expanded.getShowDate();
        long old_maxSeats = old_show_expanded.getMaxSeats();
        long old_currAvailableSeats = old_show_expanded.getCurrAvailableSeats();
        boolean old_isEnded = old_show_expanded.getIsEnded();
        String old_cinemaName = old_show_expanded.getCinemaName();
        String old_cinemaLocation = old_show_expanded.getCinemaLocation();

        long old_num_seats = old_show_expanded.getCommittedBooking();

        if (Objects.equals(is_this_a_cinema, "true")) {
            System.out.println("Show booking failed : requested by a cinema : cname " + old_cinemaName);
            return;
        }

        // the only thing that a customer can change is the number of seats !!!

        long new_booking_number = Long.parseLong(request.getParameter("new_booking_number"));


        CustomerBooking newBooking = new CustomerBooking(sender_Name, new_booking_number);

        System.out.println("DoPost Booking Update");
        System.out.println(newBooking);


        // now we try to execute the new booking

        JE_CommunicationHandler communicationHandler = new JE_CommunicationHandler();
        OtpErlangPid pid = null;
        try {
            pid = communicationHandler.getShowPidFromId(request.getSession(), old_showID);
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }


        if (pid != null) {
            System.out.println("POST Show pid got: " + pid.toString());
            // request.getSession().setAttribute("currentShowPid", pid);

            boolean updateBookingStatusResult = false;
            try {
                // we send the new booking
                ShowWithBookings newState = new JE_CommunicationHandler().send_booking_by_Customer(request.getSession(), pid, newBooking);
                updateBookingStatusResult = newState != null;
                // TODO: change the page to conform to new state

            } catch (OtpErlangDecodeException | OtpErlangExit e) {
                e.printStackTrace();
            }

            if (updateBookingStatusResult) {
                request.getSession().setAttribute("bookingUpdateStatus", "success");
                response.sendRedirect(request.getContextPath() + "/ShowPageServlet");
            } else {
                request.getSession().setAttribute("bookingUpdateStatus", "error");
                RequestDispatcher requestDispatcher = request.getRequestDispatcher("/jpages/show_page.jsp");
                requestDispatcher.forward(request, response);
            }
        }
    }

}
