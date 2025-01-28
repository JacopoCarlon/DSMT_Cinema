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

@WebServlet(name = "UserPageServlet", value = "/UserPageServlet")
public class UserPageServlet extends HttpServlet{

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("DoGet UserPageServlet");
        // get active bookings of user :
        String username = request.getParameter("username");
        try {
            List<Booking> bookingList = new JE_CommunicationHandler().get_shows_by_Customer(request.getSession(), username );
            request.setAttribute("bookingList", bookingList);
            request.getSession().setAttribute("bookingList", bookingList);
        } catch (OtpErlangExit | OtpErlangDecodeException e) {
            e.printStackTrace();
        }
        RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/pages/user_page.jsp");
        requestDispatcher.forward(request, response);
    }


    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here by pressing on a show from userPage -> want to see the show page

        System.out.println("doPost UserPageServlet");
        boolean isJoiningOkay = true;


        String username = request.getParameter("username");
        String showID = request.getParameter("showID");
        String showName = request.getParameter("showName");
        String cinemaName = request.getParameter("cinemaName");
        String showDate = request.getParameter("showDate");
        long num_seats = Long.parseLong( request.getParameter("num_seats") ) ;

        String session_is_this_a_cinema = (String) request.getSession().getAttribute("is_a_cinema");
        String session_name = (String) request.getSession().getAttribute("username");

        Booking selectedBooking = new Booking(username, showID, showName, cinemaName, showDate, num_seats);

        System.out.println("Selected booking: " + selectedBooking);

        JE_CommunicationHandler communicationHandler = new JE_CommunicationHandler();
        OtpErlangPid pid = null;
        try {
            pid = communicationHandler.getShowPidFromBooking(request.getSession(), selectedBooking);
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }

        if(pid != null){
            System.out.println("Show pid got: " + pid.toString());
            request.getSession().setAttribute("currentBooking", selectedBooking);
            request.getSession().setAttribute("currentShowPid", pid);

            boolean good_to_move = false;
            try {
                // we get the updated show data
                ShowExpanded updatedShowExpanded = new JE_CommunicationHandler().getShowExpandedUpdated(request.getSession() , pid , session_is_this_a_cinema, session_name);
                if (updatedShowExpanded != null){
                    request.getSession().setAttribute("currentShowExpanded", updatedShowExpanded);
                    good_to_move = true;
                }
            } catch (OtpErlangDecodeException | OtpErlangExit e) {
                e.printStackTrace();
            }

            if(good_to_move ){
                System.out.println("go to show page");
                response.sendRedirect(request.getContextPath() + "/ShowServlet");
            }else {
                request.getSession().removeAttribute("currentBooking");
                request.getSession().removeAttribute("currentShowPid");
                RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/pages/user_page.jsp");
                requestDispatcher.forward(request, response);
            }
        }
    }

}
