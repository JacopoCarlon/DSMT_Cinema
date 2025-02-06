package org.example.CinemaBooking.servlets;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.example.CinemaBooking.communication.JE_CommunicationHandler;
import org.example.CinemaBooking.dto.Customer;
import org.example.CinemaBooking.dto.Cinema;

import java.io.IOException;

@WebServlet(name = "CinemaLoginServlet", value = "/CinemaLoginServlet")
public class CinemaLoginServlet extends HttpServlet {
    private final String targetJSP = "/jpages/cinema_login.jsp";

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here to login from a fresh browser, or in order to do a logout
        // registrationStatus attribute has been removed in doGet of RegistrationServlet
        System.out.println("doGet CinemaLogin");
        request.getSession().removeAttribute("username");
        request.getSession().removeAttribute("is_a_cinema");
        request.getSession().removeAttribute("loginStatus");
        RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
        requestDispatcher.forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here from POST in index.jsp (login page)
        Long cinemaID = Long.parseLong(request.getParameter("username"));
        String password = request.getParameter("password");
        System.out.println("DoPost CinemaLogin");
        System.out.println("CinemaID: " + cinemaID + " - Password: " + password);

        boolean isLoginOkay = false;
        try {
            isLoginOkay = new JE_CommunicationHandler().loginExistingCinema(request.getSession(), cinemaID, password);
        }
        catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }

        if (isLoginOkay) {
            // cinemas have username used as cinemaID (since they will insert cinemaID for login).
            request.getSession().setAttribute("is_a_cinema", "true");
            request.getSession().setAttribute("username", cinemaID);
            request.getSession().removeAttribute("loginStatus");
            System.out.println("Login success");
            response.sendRedirect(request.getContextPath() + "/CinemaPageServlet?cinemaID=" + cinemaID);
        } else {
            System.out.println("Sign in failed");
            request.getSession().setAttribute("loginStatus", "error");
            RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
            requestDispatcher.forward(request, response);
        }
    }
}
