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

@WebServlet(name = "UserPageServlet", value = "/UserPageServlet")
public class UserPageServlet extends HttpServlet{

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("DoGet UserPageServlet");
        String is_a_cinema = (String) request.getSession().getAttribute("is_a_cinema");
        if (Objects.equals(is_a_cinema, "true")) {
            // if it is a cinema, should NOT go to user personal page, is sent back to own cinema page!
            Long cinemaID = (Long) request.getSession().getAttribute("username");
            response.sendRedirect(request.getContextPath() + "/CinemaPageServlet?cinemaID=" + cinemaID);
        }else if (Objects.equals(is_a_cinema, "false")) {
            // get active bookings of user :
            String username = (String) request.getSession().getAttribute("username");
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
        }else{
            request.getSession().setAttribute("errorMsg", "ERROR: is_a_cinema parameter is wrong");

            RequestDispatcher requestDispatcher = request.getRequestDispatcher("/jpages/error.jsp");
            requestDispatcher.forward(request, response);
        }
        return;
    }



    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here by pressing on a show from userPage -> want to see the show page
        System.out.println("doPost UserPageServlet");

        Long showID = Long.parseLong( request.getParameter("showID") );

        request.getSession().removeAttribute("getShowID");
        request.getSession().setAttribute("getShowID", showID);

        // we go straight to a get in the ShowServlet of show_page.jsp !
        System.out.println("go to show page");
        response.sendRedirect(request.getContextPath() + "/ShowPageServlet");
        return;
    }


}
