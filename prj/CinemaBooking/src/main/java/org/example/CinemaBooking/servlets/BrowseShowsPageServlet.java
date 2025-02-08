package org.example.CinemaBooking.servlets;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.example.CinemaBooking.communication.JE_CommunicationHandler;
import org.example.CinemaBooking.dto.Show;

import java.io.IOException;
import java.util.List;

@WebServlet(name = "BrowseShowsPageServlet", value = "/BrowseShowsServlet")
public class BrowseShowsPageServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        boolean includeOldShows = false;
        if (request.getParameter("includeOldShows") != null) {
            includeOldShows = Boolean.parseBoolean(request.getParameter("includeOldShows"));
        }

        System.out.println("DoGet BrowseShowsPageServlet : try to get list of available shows");
        try {
            List<Show> showList = new JE_CommunicationHandler().getListOfShows(request.getSession(), includeOldShows);
            request.removeAttribute("allShowsList");
            request.setAttribute("allShowsList", showList);
        } catch (OtpErlangExit | OtpErlangDecodeException e) {
            e.printStackTrace();
        }

        RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/jpages/browse_shows_page.jsp");
        requestDispatcher.forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("doPost BrowseShowsPageServlet");

        Long showID = Long.parseLong( request.getParameter("showID") );
        request.getSession().removeAttribute("getShowID");
        request.getSession().setAttribute("getShowID", showID);

        // we go straight to a get in the ShowServlet of show_page.jsp !
        System.out.println("go to show page");
        response.sendRedirect(request.getContextPath() + "/ShowPageServlet");
    }
}
