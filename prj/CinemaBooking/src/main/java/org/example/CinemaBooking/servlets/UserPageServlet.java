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
        // todo : add check is user not cinema
        // get active bookings of user :
        String username = (String) request.getSession().getAttribute("username");
        String is_a_cinema = request.getParameter("is_a_cinema");
        System.out.println("DoGet UserPageServlet : try to get shows of user : " + username + " which is cinema ? : " + is_a_cinema);
        try {
            List<ShowWithBookings> showWithBookingsList = new JE_CommunicationHandler().get_shows_by_Customer(request.getSession(), username );
            request.setAttribute("showWithBookingsList", showWithBookingsList);
            // request.getSession().setAttribute("bookingList", bookingList);
        } catch (OtpErlangExit | OtpErlangDecodeException e) {
            e.printStackTrace();
        }

        RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/jpages/user_page.jsp");
        requestDispatcher.forward(request, response);
    }



    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here by pressing on a show from userPage -> want to see the show page
        System.out.println("doPost UserPageServlet");

        // String username = request.getParameter("username");
        Long showID = Long.parseLong( request.getParameter("showID") );
        /*
        String showName = request.getParameter("showName");
        String cinemaName = request.getParameter("cinemaName");
        String showDate = request.getParameter("showDate");
        long num_seats = Long.parseLong( request.getParameter("num_seats") ) ;
        String session_is_this_a_cinema = (String) request.getSession().getAttribute("is_a_cinema");
        String session_name = (String) request.getSession().getAttribute("username");
        */
        request.getSession().removeAttribute("getShowID");
        request.getSession().setAttribute("getShowID", showID);

        // we go straight to a get in the ShowServlet of show_page.jsp !
        System.out.println("go to show page");
        response.sendRedirect(request.getContextPath() + "/ShowPageServlet");
        return;
    }


}
