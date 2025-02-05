package org.example.CinemaBooking.servlets;

import org.example.CinemaBooking.dto.Show;

import org.example.CinemaBooking.communication.JE_CommunicationHandler;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Objects;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangPid;

@WebServlet(name = "CreateShowServlet", value = "/CreateShowServlet")
public class CreateShowServlet extends HttpServlet{

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here from A_jump from cinema_page.jsp ... -> just load the page bro
        String targetJSP = "/pages/create_show.jsp";
        request.getSession().removeAttribute("showCreationStatus");
        RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
        requestDispatcher.forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String is_this_a_cinema = (String) request.getSession().getAttribute("is_a_cinema");
        Long cinemaID = (Long) request.getSession().getAttribute("username");
        if (!Objects.equals(is_this_a_cinema, "true")){
            System.out.println("Show creation failed : requested by something that is not a cinema : uname" + cinemaID);
            return;
        }

        // now we positive the post request came from a cinema !

        // # __showID will be given by server
        // # __showName
        String showName = request.getParameter("showName");
        // # __maxSeats
        // // <input type="datetime-local" id="showDate" name="showDate" value="2025-11-11T11:11" min="2025-01-01T00:01" max="2150-12-31T11:59" required/>
        // -> date string format is : "yyyy-mm-ddThh:mm" .
        String showDate = request.getParameter("showDate");
        // # __maxSeats
        long maxSeats = Long.parseLong( request.getParameter("maxSeats") ) ;
        // # __currAvailableSeats == maxSeats;
        long currAvailableSeats = maxSeats;
        // # __isEnded == false.

        Show newShow = new Show(showName, showDate, maxSeats, currAvailableSeats);

        System.out.println("DoPost Show Creation");
        System.out.println("showName: " + showName + "\nshowDate: " + showDate + "\nmaxSeats: " + maxSeats);

        Long new_id = null;
        try {
            new_id = new JE_CommunicationHandler().createNewShowForCinema(request.getSession(), cinemaID, newShow);
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }

        if (new_id >= 0) {
            boolean isJoiningOkay = false;
            System.out.println("Show creation succeded, got id: " + new_id);
            // TODO: add cinema details to construct Show
            // Show updatedShow = new Show(new_id, showName, showDate, maxSeats, currAvailableSeats);
            request.getSession().setAttribute("showCreationStatus", "success");
            // request.getSession().setAttribute("createdShow", updatedShow);
            // request.getSession().setAttribute("currentShowPid", pid);
            RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/pages/cinema_page.jsp");
            requestDispatcher.forward(request, response);
        } else {
            System.out.println("Show creation failed");
            request.getSession().setAttribute("showCreationStatus", "error");
            RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/pages/create_show.jsp");
            requestDispatcher.forward(request, response);
        }
    }





}

