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

@WebServlet(name = "LoginServlet", value = "/LoginServlet")
public class LoginServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here to login from a fresh browser, or after a logout
        // registrationStatus attribute has been removed in doGet of RegistrationServlet
        System.out.println("doGet Login");
        request.getSession().removeAttribute("username");
        request.getSession().removeAttribute("loginStatus");
        String targetJSP = "/index.jsp";
        RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
        requestDispatcher.forward(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // arrive here from POST in index.jsp (login page)
        String username = request.getParameter("username");
        String password = request.getParameter("password");
        boolean is_a_cinema = Boolean.parseBoolean( request.getParameter("is_cinema") );
        System.out.println("DoPost Login");
        System.out.println("username: " + username + "password: " + password + "is_a_cinema: " + is_a_cinema);

        boolean isLoginOkay = false;
        if(is_a_cinema){
            try {
                isLoginOkay = new JE_CommunicationHandler().loginExistingCinema(request.getSession(), username, password);
            } catch (OtpErlangDecodeException | OtpErlangExit e) {
                e.printStackTrace();
            }
        }else{
            try {
                isLoginOkay = new JE_CommunicationHandler().loginExistingCustomer(request.getSession(), username, password);
            } catch (OtpErlangDecodeException | OtpErlangExit e) {
                e.printStackTrace();
            }
        }

        if (isLoginOkay) {
            request.getSession().setAttribute("username", username);
            request.getSession().setAttribute("is_a_cinema", is_a_cinema);
            // request.getSession().setAttribute("is_a_cinema", (is_a_cinema)?"true":"false");
            request.getSession().removeAttribute("loginStatus");
            System.out.println("Login success");
            response.sendRedirect(request.getContextPath() + "/MainMenuServlet");
        } else {
            System.out.println("Sign in failed");
            request.getSession().setAttribute("loginStatus", "error");
            RequestDispatcher requestDispatcher = request.getRequestDispatcher("/index.jsp");
            requestDispatcher.forward(request, response);
        }

    }
}
