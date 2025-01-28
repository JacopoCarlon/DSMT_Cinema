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
        // ShowExpanded is already in :
        // request.getSession().setAttribute("currentShowExpanded", updatedShowExpanded);
        String targetJSP = "/pages/show_page.jsp";
        request.getSession().removeAttribute("bookingUpdateStatus");
        RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
        requestDispatcher.forward(request, response);
    }


    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String is_this_a_cinema = (String) request.getSession().getAttribute("is_a_cinema");
        String sender_Name = (String) request.getSession().getAttribute("username");

        ShowExpanded old_show_expanded = (ShowExpanded) request.getSession().getAttribute("currentShowExpanded");
        String old_showID = old_show_expanded.getShowID();
        String old_showName = old_show_expanded.getShowName();
        String old_showDate = old_show_expanded.getShowDate();
        long old_maxSeats = old_show_expanded.getMaxSeats();
        long old_currAvailableSeats = old_show_expanded.getCurrAvailableSeats();
        boolean old_isEnded = old_show_expanded.getEnded();
        String old_cinemaName = old_show_expanded.getCinemaName();
        String old_cinemaLocation = old_show_expanded.getCinemaLocation();
        String old_username = old_show_expanded.getUsername();
        long old_num_seats = old_show_expanded.getNumSeats();
        String old_is_a_cinema = old_show_expanded.getIs_a_cinema();

        if (Objects.equals(is_this_a_cinema, "true")) {
            System.out.println("Show booking failed : requested by a cinema : cname " + old_cinemaName);
            return;
        }

        // the only thing that a customer can change is the number of seats !!!

        long new_booking_number = Long.parseLong(request.getParameter("new_booking_number"));


        Booking oldBooking = new Booking(old_username, old_showID, old_showName, old_cinemaName, old_showDate, old_num_seats);
        Booking newBooking = new Booking(old_username, old_showID, old_showName, old_cinemaName, old_showDate, new_booking_number);

        System.out.println("DoPost Booking Update");
        System.out.println(newBooking.toString());


        // now we try to execute the new booking

        JE_CommunicationHandler communicationHandler = new JE_CommunicationHandler();
        OtpErlangPid pid = null;
        try {
            pid = communicationHandler.getShowPidFromBooking(request.getSession(), oldBooking);
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }


        if (pid != null) {
            System.out.println("Show pid got: " + pid.toString());
            request.getSession().setAttribute("currentShowPid", pid);

            boolean updateBookingStatusResult = false;
            try {
                // we send the new booking
                updateBookingStatusResult = new JE_CommunicationHandler().send_booking_by_Customer(request.getSession(), newBooking);

            } catch (OtpErlangDecodeException | OtpErlangExit e) {
                e.printStackTrace();
            }

            if (updateBookingStatusResult) {
                request.getSession().setAttribute("bookingUpdateStatus", "success");
                response.sendRedirect(request.getContextPath() + "/ShowPageServlet");
            } else {
                request.getSession().setAttribute("bookingUpdateStatus", "error");
                RequestDispatcher requestDispatcher = request.getRequestDispatcher("/pages/show_page.jsp");
                requestDispatcher.forward(request, response);
            }
        }
    }

}
