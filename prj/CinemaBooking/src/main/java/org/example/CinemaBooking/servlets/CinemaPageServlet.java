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

import org.example.CinemaBooking.dto.Cinema;
import org.example.CinemaBooking.dto.Show;


import java.io.IOException;
import java.util.List;

@WebServlet(name = "CinemaPageServlet", value = "/CinemaPageServlet")
public class CinemaPageServlet extends HttpServlet  {

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("DoGet CinemaPageServlet");

        Long cinemaID = Long.parseLong(request.getParameter("cinemaID"));
        boolean is_a_cinema = Boolean.parseBoolean((String) request.getSession().getAttribute("is_a_cinema"));
        System.out.println("DoGet CinemaPageServlet : try to get shows of cinema : " + cinemaID + " which is cinema ? : " + is_a_cinema);
        try {
            JE_CommunicationHandler jec = new JE_CommunicationHandler();
            Cinema cinemaInfo = jec.getCinema(request.getSession(), cinemaID);
            request.setAttribute("cinemaInfo", cinemaInfo);

            if (cinemaInfo != null) {
                List<Show> showList = jec.get_shows_by_cinema(request.getSession(), cinemaID);
                request.setAttribute("showList", showList);
            }
            // request.getSession().setAttribute("showList", showList);
        } catch (OtpErlangExit | OtpErlangDecodeException e) {
            e.printStackTrace();
        }

        RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/jpages/cinema_page.jsp");
        requestDispatcher.forward(request, response);
    }



    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here by pressing on a show from userPage -> want to see the show page
        System.out.println("doPost UserPageServlet");

        // String username = request.getParameter("username");
        Long showID = Long.parseLong( request.getParameter("showID") );

        request.getSession().removeAttribute("getShowID");
        request.getSession().setAttribute("getShowID", showID);

        // we go straight to a get in the ShowServlet of show_page.jsp !
        System.out.println("go to show page");
        response.sendRedirect(request.getContextPath() + "/ShowPageServlet");
        return;
    }


}



