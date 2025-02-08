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

@WebServlet(name = "CreateShowServlet", value = "/CreateShowServlet")
public class CreateShowServlet extends HttpServlet{

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here from A_jump from cinema_page.jsp ... -> just load the page bro
        System.out.println("DoGet CreateShowServlet");
        String is_a_cinema = request.getParameter("is_a_cinema");

        // MUST BE A CINEMA
        if (Objects.equals(is_a_cinema, "true")) {
            // is a cinema : OK
            request.getSession().removeAttribute("showCreationStatus");
            RequestDispatcher requestDispatcher = request.getRequestDispatcher("/jpages/create_show.jsp");
            requestDispatcher.forward(request, response);
        }else if(Objects.equals(is_a_cinema, "false")){
            // is a user -> back to user page
            Long cinemaID = (Long) request.getSession().getAttribute("username");
            response.sendRedirect(request.getContextPath() + "/UserPageServlet");
        }else{
            // error : unlogged, or smt bad happened >:-/
            request.getSession().setAttribute("errorMsg", "ERROR: is_a_cinema parameter is wrong");
            RequestDispatcher requestDispatcher = request.getRequestDispatcher("/jpages/error.jsp");
            requestDispatcher.forward(request, response);
        }
        return;
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        boolean is_a_cinema = request.getSession().getAttribute("is_a_cinema") == "true";
        if (!is_a_cinema){
            System.out.println("Show creation failed : requested by something that is not a cinema : " + request.getSession().getAttribute("username"));
            return;
        }

        Long cinemaID = (Long) request.getSession().getAttribute("username");

        // # __showID will be given by server
        // # __showName
        String showName = request.getParameter("showName");
        // # __maxSeats
        // // <input type="datetime-local" id="showDate" name="showDate" value="2025-11-11T11:11" min="2025-01-01T00:01" max="2150-12-31T11:59" required/>
        // -> date string format is : "yyyy-mm-ddThh:mm" .
        String showDate = request.getParameter("showDate");
        // # __maxSeats
        long maxSeats = Long.parseLong( request.getParameter("maxSeats") ) ;

        Show newShow = new Show(showName, showDate, cinemaID, maxSeats);

        System.out.println("DoPost Show Creation");
        System.out.println("showName: " + showName + "\nshowDate: " + showDate + "\nmaxSeats: " + maxSeats);

        Long new_id = null;
        try {
            new_id = new JE_CommunicationHandler().createNewShowForCinema(request.getSession(), newShow);
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }

        if (new_id != null && new_id >= 0) {
            boolean isJoiningOkay = false;
            System.out.println("Show creation succeded, got id: " + new_id);
            request.getSession().setAttribute("showCreationStatus", "success");
        } else {
            System.out.println("Show creation failed");
            request.getSession().setAttribute("showCreationStatus", "error");
        }

        // go back to page
        RequestDispatcher requestDispatcher = request.getRequestDispatcher( "/jpages/create_show.jsp");
        requestDispatcher.forward(request, response);
    }





}

