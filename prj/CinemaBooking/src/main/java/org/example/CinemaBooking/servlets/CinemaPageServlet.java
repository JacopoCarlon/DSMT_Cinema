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
        // todo : add check is cinema not user
        // String username = request.getParameter("username");
        Long cinemaID = (Long) request.getSession().getAttribute("username");
        String is_a_cinema = request.getParameter("is_a_cinema");
        System.out.println("DoGet CinemaPageServlet : try to get shows of cinema : " + cinemaID + " which is cinema ? : " + is_a_cinema);
        try {
            List<Show> showList = new JE_CommunicationHandler().get_shows_by_cinema(request.getSession(), cinemaID );
            request.setAttribute("showList", showList);
            // request.getSession().setAttribute("showList", showList);
        } catch (OtpErlangExit | OtpErlangDecodeException e) {
            e.printStackTrace();
        }

        RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/pages/cinema_page.jsp");
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
        response.sendRedirect(request.getContextPath() + "/ShowServlet");
        return;
    }


}



