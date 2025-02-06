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

import java.io.IOException;

@WebServlet(name = "CinemaRegistrationServlet", value = "/CinemaRegistrationServlet")
public class CinemaRegistrationServlet extends HttpServlet {
    private final String targetJSP = "/jpages/cinema_registration.jsp";
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("doGet CinemaRegistration");
        request.getSession().removeAttribute("registrationStatus");

        RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
        requestDispatcher.forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String cinemaName = request.getParameter("cinemaName");
        String password = request.getParameter("password");
        String cinemaLocation = request.getParameter("cinemaLocation");

        System.out.println("DoPost CinemaRegistration");
        System.out.println("Name: " + cinemaName + " - password: " + password + " - Location: " + cinemaLocation);

        Long cinemaID = -1L;
        try {
            cinemaID = new JE_CommunicationHandler().registerNewCinema(request.getSession(), cinemaName, password, cinemaLocation);
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }

        if (cinemaID >= 0L){
            System.out.println("Registration success");
            request.getSession().setAttribute("is_a_cinema", "true");
            request.getSession().setAttribute("username", cinemaID);
            request.getSession().removeAttribute("loginStatus");
            request.getSession().removeAttribute("registrationStatus");
            response.sendRedirect(request.getContextPath() + "/CinemaPageServlet?cinemaID=" + cinemaID);
        } else {
            System.out.println("Sign in failed");
            // show error in html
            request.getSession().setAttribute("registrationStatus", "error");
            RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
            requestDispatcher.forward(request, response);
        }
    }
}
