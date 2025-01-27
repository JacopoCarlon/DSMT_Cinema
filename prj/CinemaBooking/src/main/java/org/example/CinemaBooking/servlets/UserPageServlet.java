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

import org.example.CinemaBooking.dto.Booking;
import org.example.CinemaBooking.dto.Cinema;
import org.example.CinemaBooking.dto.Customer;
import org.example.CinemaBooking.dto.Show;


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

        RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/pages/user_private_page.jsp");
        requestDispatcher.forward(request, response);

    }
}
