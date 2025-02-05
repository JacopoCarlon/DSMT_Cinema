package org.example.CinemaBooking.servlets;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import org.example.CinemaBooking.communication.JE_CommunicationHandler;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.example.CinemaBooking.dto.Show;


import java.io.IOException;
import java.util.List;

@WebServlet(name = "CinemaPageServlet", value = "/CinemaPageServlet")
public class CinemaPageServlet extends HttpServlet  {

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("DoGet CinemaPageServlet");

        // get active shows of cinema :
        // todo : test which way should be done getting username and is_a_cinema
        // String username = request.getParameter("username");
        Long cinemaID = (Long) request.getSession().getAttribute("username");
        String is_a_cinema = request.getParameter("is_a_cinema");
        System.out.println("DoGet CinemaPageServlet : try to get shows of cinema : " + cinemaID + " which is cinema ? : " + is_a_cinema);
        try {
            List<Show> showList = new JE_CommunicationHandler().get_shows_by_cinema(request.getSession(), cinemaID );
            request.setAttribute("showList", showList);
            request.getSession().setAttribute("showList", showList);
        } catch (OtpErlangExit | OtpErlangDecodeException e) {
            e.printStackTrace();
        }

        RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/pages/cinema_page.jsp");
        requestDispatcher.forward(request, response);

    }
}



